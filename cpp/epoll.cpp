#include <iostream>
#include <sys/epoll.h>
#include <unistd.h>

int main() {
  int epoll_fd = epoll_create1(0);

  if (epoll_fd == -1) {
    return 1;
  }

  epoll_event event;
  event.events = EPOLLIN;
  event.data.fd = 0;

  if (epoll_ctl(epoll_fd, EPOLL_CTL_ADD, 0, &event)) {
    close(epoll_fd);
    return 1;
  }

  const int MAX_EVENTS = 5;
  epoll_event events[MAX_EVENTS];
  const int READ_SIZE = 10;
  char read_buffer[READ_SIZE + 1];
  while (true) {
    int num_ready_fd = epoll_wait(epoll_fd, events, MAX_EVENTS, 30000);
    for (int i = 0; i < num_ready_fd; i++) {
      size_t bytes_read = read(events[i].data.fd, read_buffer, READ_SIZE);
      read_buffer[bytes_read] = '\0';
      std::cout << read_buffer << '\n';
    }
  }

  if (close(epoll_fd)) {
    return 1;
  }

  return 0;
}
