// wayland-security-context APP_ID COMMAND [ARGUMENT...]
//
// Runs COMMAND with a Wayland socket of its own, created through the
// security-context-v1 protocol, and puts the path of that socket in
// `NIXPAK_WAYLAND_SOCKET`. The compositor knows that whoever connects through
// it is sandboxed and keeps the privileged protocols away from it: screen
// capture, virtual keyboard and pointer, clipboard managers, layer shell and
// session lock, among others. This is what Flatpak does for its applications.
//
// Unlike a Wayland proxy, the client still talks to the compositor directly,
// so `linux-dmabuf` keeps working and GPU rendering is not copied through
// shared memory.

#define _GNU_SOURCE

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <unistd.h>
#include <wayland-client.h>

#include "security-context-v1-client-protocol.h"

#define PROG_NAME "wayland-security-context"
#define UNUSED(x) (void) (x)

static struct wp_security_context_manager_v1 *manager = NULL;
static pid_t child = 0;

// Prints `msg` followed by the description of `errno` and exits.
static noreturn void die(const char *msg)
{
    fprintf(stderr, "%s: %s: %s\n", PROG_NAME, msg, strerror(errno));
    exit(1);
}

static void handle_global(void *data, struct wl_registry *registry,
                          uint32_t name, const char *interface, uint32_t version)
{
    UNUSED(data);
    UNUSED(version);

    if (!strcmp(interface, wp_security_context_manager_v1_interface.name))
        manager = wl_registry_bind(registry, name,
                                   &wp_security_context_manager_v1_interface, 1);
}

static void handle_global_remove(void *data, struct wl_registry *registry,
                                 uint32_t name)
{
    UNUSED(data);
    UNUSED(registry);
    UNUSED(name);
}

static const struct wl_registry_listener registry_listener = {
    .global = handle_global,
    .global_remove = handle_global_remove,
};

static void forward_signal(int sig)
{
    if (child > 0) kill(child, sig);
}

int main(int argc, char **argv)
{
    if (argc < 3) {
        fprintf(stderr, "Usage: %s APP_ID COMMAND [ARGUMENT...]\n", PROG_NAME);
        return 2;
    }
    const char *app_id = argv[1];
    char **command = argv + 2;

    const char *runtime_dir = getenv("XDG_RUNTIME_DIR");
    if (!runtime_dir || runtime_dir[0] != '/') {
        fprintf(stderr, "%s: XDG_RUNTIME_DIR is not set\n", PROG_NAME);
        return 1;
    }

    char instance_id[32];
    snprintf(instance_id, sizeof(instance_id), "%ld", (long) getpid());

    struct sockaddr_un addr = { .sun_family = AF_UNIX };
    int len = snprintf(addr.sun_path, sizeof(addr.sun_path),
                       "%s/nixpak-wayland-%s", runtime_dir, instance_id);
    if (len < 0 || (size_t) len >= sizeof(addr.sun_path)) {
        fprintf(stderr, "%s: socket path is too long\n", PROG_NAME);
        return 1;
    }

    struct wl_display *display = wl_display_connect(NULL);
    if (!display)
        die("cannot connect to the compositor");

    struct wl_registry *registry = wl_display_get_registry(display);
    wl_registry_add_listener(registry, &registry_listener, NULL);
    if (wl_display_roundtrip(display) < 0)
        die("cannot list the globals of the compositor");
    // Never fall back to the socket of the compositor itself
    if (!manager) {
        fprintf(stderr, "%s: the compositor does not support security-context-v1\n",
                PROG_NAME);
        return 1;
    }

    int listen_fd = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
    if (listen_fd < 0)
        die("cannot create the socket");
    unlink(addr.sun_path);
    if (bind(listen_fd, (struct sockaddr *) &addr, sizeof(addr)) < 0)
        die("cannot bind the socket");
    if (listen(listen_fd, SOMAXCONN) < 0)
        die("cannot listen on the socket");

    // The compositor accepts connections until the read end hangs up, which
    // happens once every process holding the write end is gone. The write end
    // is inherited by COMMAND and its children on purpose, so that the socket
    // keeps working for as long as any of them is alive.
    int close_fds[2];
    if (pipe(close_fds) < 0)
        die("cannot create the pipe");

    struct wp_security_context_v1 *context =
        wp_security_context_manager_v1_create_listener(manager, listen_fd,
                                                       close_fds[0]);
    wp_security_context_v1_set_sandbox_engine(context, "io.github.nixpak");
    wp_security_context_v1_set_app_id(context, app_id);
    wp_security_context_v1_set_instance_id(context, instance_id);
    wp_security_context_v1_commit(context);
    wp_security_context_v1_destroy(context);
    if (wl_display_roundtrip(display) < 0) {
        unlink(addr.sun_path);
        die("the compositor rejected the security context");
    }

    // The listener outlives this connection
    wp_security_context_manager_v1_destroy(manager);
    wl_registry_destroy(registry);
    wl_display_disconnect(display);
    close(listen_fd);
    close(close_fds[0]);

    if (setenv("NIXPAK_WAYLAND_SOCKET", addr.sun_path, 1) < 0)
        die("cannot set NIXPAK_WAYLAND_SOCKET");

    // Installed before forking so that no signal is lost; exec resets them
    struct sigaction action = { .sa_handler = forward_signal };
    sigemptyset(&action.sa_mask);
    sigaction(SIGHUP, &action, NULL);
    sigaction(SIGINT, &action, NULL);
    sigaction(SIGTERM, &action, NULL);

    child = fork();
    if (child < 0)
        die("cannot fork");
    if (child == 0) {
        execvp(command[0], command);
        fprintf(stderr, "%s: cannot run %s: %s\n", PROG_NAME, command[0],
                strerror(errno));
        _exit(127);
    }

    int status = 0;
    while (waitpid(child, &status, 0) < 0)
        if (errno != EINTR)
            die("cannot wait for the command");

    // Sandboxes that are still running keep the socket through their mount
    unlink(addr.sun_path);

    if (WIFSIGNALED(status))
        return 128 + WTERMSIG(status);
    return WEXITSTATUS(status);
}
