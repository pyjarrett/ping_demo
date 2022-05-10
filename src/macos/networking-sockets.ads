with Interfaces.C;
with Networking.Types;  use Networking.Types;
with System.Address_To_Access_Conversions;

package Networking.Sockets is
   package Socket_Address_Conversions is new System.Address_To_Access_Conversions(sockaddr);
   subtype sockaddr_ptr is Socket_Address_Conversions.Object_Pointer;
   pragma Assert(sockaddr_ptr'Size = 64);

   --  int getaddrinfo(
   --      const char *hostname,
   --      const char *servname,
   --      const struct addrinfo *hints,
   --      struct addrinfo **res);
   -- getaddrinfo(target, nullptr, &hints, &m_alternatives);
   function getaddrinfo(
      Target : Interfaces.C.Strings.chars_ptr;
      Unused : Interfaces.C.Strings.chars_ptr;
      Hints  : addrinfo_ptr;
      Result : System.Address
   ) return Interfaces.C.Int
      with Import, Convention => Stdcall, External_Name => "getaddrinfo";

   -- man socket
   --
   --  #include <sys/socket.h>
   --  int socket(int domain, int type, int protocol);
   --
   -- A -1 is returned if an error occurs, otherwise the return value is a
   -- descriptor referencing the socket.
   function socket(
      Communication_Domain    : Protocol_Family;
      Communication_Semantics : Socket_Type;
      Protocol                : Socket_Protocol
   ) return Socket_Descriptor
      with Import, Convention => C;

   -- int connect(int socket,
   --             const struct sockaddr *address,
   --             socklen_t address_len);
   function connect (
      Socket         : Socket_Descriptor;
      Address        : System.Address;
      Address_Length : socklen_t
   ) return Connect_Status
      with Import, Convention => C;

   function close (File_Descriptor : int) return int
      with Import, Convention => C;

   -- Hide the return value of close() when we don't need it.
   procedure close (File_Descriptor : int);

   -- #include <sys/socket.h>
   -- ssize_t
   -- send(int socket, const void *buffer, size_t length, int flags);
   function send (
      Socket : Socket_Descriptor;
      Buffer : Void_Ptr;
      Length : Interfaces.C.size_t;
      Flags  : int
   ) return ssize_t
      with Import, Convention => C;


   -- #include <sys/socket.h>
   -- ssize_t
   -- recv(int socket, void *buffer, size_t length, int flags);
   function recv (
      Socket : Socket_Descriptor;
      Buffer : Void_Ptr;
      Length : Interfaces.C.size_t;
      Flags : int
   ) return ssize_t
      with Import, Convention => C;    

   -- https://pubs.opengroup.org/onlinepubs/009696799/functions/poll.html
   -- /usr/include/sys/poll.h
   --  struct pollfd {
   --      int    fd;       /* file descriptor */
   --      short  events;   /* events to look for */
   --      short  revents;  /* events returned */
   --  };
   type pollfd is record
      fd      : Interfaces.C.int;
      events  : Poll_Events;
      revents : Poll_Events;
   end record
      with Convention => C;

   type pollfd_array is array (Positive range <>) of pollfd with Convention => C;

   -- /usr/include/sys/poll.h
   -- typedef unsigned int nfds_t;
   type Num_FDs is new Interfaces.C.unsigned;

   -- /usr/include/sys/poll.h
   --  int poll(struct pollfd fds[], nfds_t nfds, int timeout);
   function poll (
      fds     : in out pollfd_array;
      nfds    : Num_FDs;
      timeout : Interfaces.C.int
   ) return Interfaces.C.int
      with Import, Convention => C;

   function Image (Self : addrinfo) return String;
   function Image (Ptr : Socket_Address_Conversions.Object_Pointer) return String;

end Networking.Sockets;
