#include <sys/socket.h>
#include <unistd.h>
#include <netdb.h>
#include <cstring>
#include <iostream>

int main() {

  addrinfo hints;
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;     /* Allow IPv4 or IPv6 */
  hints.ai_socktype = SOCK_STREAM; /* TCP stream sockets */
  hints.ai_flags = AI_PASSIVE;     /* For wildcard IP address */
  hints.ai_protocol = 0;           /* Any protocol */
  hints.ai_canonname = NULL;
  hints.ai_addr = NULL;
  hints.ai_next = NULL;

  addrinfo* result;
  const char* port = "8080";
  int ret = getaddrinfo(nullptr, port, &hints, &result);
  if (ret != 0) {
    std::cout << "getaddrinfo: " << gai_strerror(ret) << "\n";
    return 1;
  }

  addrinfo* rp;  // result pointer
  int accept_fd;
  for (rp = result; rp != nullptr; rp = rp->ai_next) {
    accept_fd = socket(rp->ai_family,
                       rp->ai_socktype,
                       rp->ai_protocol);
    if (accept_fd == -1) {
      continue;
    }
    int yes = 1;
    if (setsockopt(accept_fd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes) == -1) {
      std::cout << "Failed to set SO_REUSEADDR" << "\n";
      return 1;
    }
    if (bind(accept_fd, rp->ai_addr, rp->ai_addrlen) == 0) {
      break;  // success
    }
    close(accept_fd);
  }
  if (rp == nullptr) {
    std::cout << "Could not bind" << "\n";
    return 1;
  }
  freeaddrinfo(result);

  const int MAX_PENDING_CONNECTIONS = 10;
  if (listen(accept_fd, MAX_PENDING_CONNECTIONS) == -1) {
    std::cout << "Failed to listen" << "\n";
    return 1;
  }

  std::cout << "Waiting for connections..." << "\n";

  while (true) {
    struct sockaddr_storage their_addr;
    socklen_t sin_size = sizeof their_addr;
    int socket_fd = accept(accept_fd,
                           (struct sockaddr *)&their_addr,
                           &sin_size);
    if (socket_fd == -1) {
      std::cout << "Failed to accept" << "\n";
      continue;
    }
    std::cout << "Got a connection" << "\n";

    while (true) {
      const int RECV_SIZE = 10;
      char recv_buffer[RECV_SIZE + 1];
      int num_bytes_received = recv(socket_fd, recv_buffer, RECV_SIZE, 0);
      if (num_bytes_received == 0) {
        std::cout << "Client closed connection" << "\n";
        break;
      }
      std::cout << "Num bytes received: " << num_bytes_received << "\n";
      recv_buffer[num_bytes_received] = '\0';
      std::cout << "-----------------------" << "\n";
      std::cout << recv_buffer;
      std::cout << "\n" << "-----------------------" << "\n";

      if (std::strchr(recv_buffer, '\n') != NULL) {
        int total_num_bytes_sent = 0;
        const int num_bytes_to_send = num_bytes_received;
        while (total_num_bytes_sent != num_bytes_to_send) {
          int num_bytes_sent = send(socket_fd, recv_buffer, num_bytes_received, 0);
          std::cout << "Num bytes sent: " << num_bytes_sent << "\n";
          if (num_bytes_sent == -1) {
            std::cout << "Error while sending" << "\n";
            return 1;
          }
          total_num_bytes_sent += num_bytes_sent;
        }
        close(socket_fd);
        break;
      }
    }
  }
}
