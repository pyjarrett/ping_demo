with Interfaces.C;
with System.Address_To_Access_Conversions;

-- The basic networking types to make the rest of the code go.
package Networking.Types is
   use type Interfaces.C.int;

   subtype Connect_Status is int;

   Connect_Error   : constant Connect_Status := -1;
   Connect_Success : constant Connect_Status := 0;

   subtype Send_Status is ssize_t;
   Send_Error   : constant Send_Status := -1;
   Send_Success : constant Send_Status := 0;

   -- C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um\WinSock2.h
   -- typedef UINT_PTR        SOCKET;
   subtype Socket_Descriptor is int;
   Invalid_Socket : constant Socket_Descriptor := -1;

   type Socket_Protocol is new int;
   type Protocol_Family is new int;

    -- /*
    --  * Types
    --  */
    -- #define SOCK_STREAM     1               /* stream socket */
    -- #define SOCK_DGRAM      2               /* datagram socket */
    -- #define SOCK_RAW        3               /* raw-protocol interface */
    -- #if !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE)
    -- #define SOCK_RDM        4               /* reliably-delivered message */
    -- #endif  /* (!_POSIX_C_SOURCE || _DARWIN_C_SOURCE) */
    -- #define SOCK_SEQPACKET  5               /* sequenced packet stream */
    subtype Socket_Type is int;
    SOCK_RAW : constant Socket_Type := 3;


    -- /usr/include/sys/sockets.h
    -- struct sockaddr {
    -- 	__uint8_t       sa_len;         /* total length */
    -- 	sa_family_t     sa_family;      /* [XSI] address family */
    -- 	char            sa_data[14];    /* [XSI] addr value (actually larger) */
    -- };
    --
    -- /usr/include/sys/_types/_sa_family_t.h
    -- typedef __uint8_t               sa_family_t;
    type sa_data_t is array (Natural range 0 .. 13) of Character with Convention => C;
    type sockaddr is record
        sa_len    : Interfaces.Unsigned_8;
        sa_family : Interfaces.Unsigned_8;
        sa_data   : sa_data_t;
    end record;

   package Socket_Address_Conversions is new System.Address_To_Access_Conversions(sockaddr);
   subtype sockaddr_ptr is Socket_Address_Conversions.Object_Pointer;
   pragma Assert(sockaddr_ptr'Size = 64);

   subtype socklen_t is Interfaces.C.size_t;

   subtype addrinfo_ptr is System.Storage_Elements.Integer_Address;
   type ai_flags_t is mod 2 ** int'Size;

   --   C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared\ws2def.h  
   -- //
   -- //  Structure used in getaddrinfo() call
   -- //
   -- typedef struct addrinfo
   -- {
   --     int                 ai_flags;       // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
   --     int                 ai_family;      // PF_xxx
   --     int                 ai_socktype;    // SOCK_xxx
   --     int                 ai_protocol;    // 0 or IPPROTO_xxx for IPv4 and IPv6
   --     size_t              ai_addrlen;     // Length of ai_addr
   --     char *              ai_canonname;   // Canonical name for nodename
   --     _Field_size_bytes_(ai_addrlen) struct sockaddr *   ai_addr;        // Binary address
   --     struct addrinfo *   ai_next;        // Next structure in linked list
   -- }
   type addrinfo is record
      ai_flags     : ai_flags_t          := 0;
      ai_family    : Protocol_Family     := 0;
      ai_socktype  : Socket_Type         := 0;
      ai_protocol  : Socket_Protocol     := 0;
      ai_addrlen   : socklen_t           := 0;
      ai_canonname : chars_ptr           := Interfaces.C.Strings.Null_Ptr;
      ai_addr      : System.Address      := System.Null_Address;
      ai_next      : System.Address      := System.Null_Address;
   end record
      with Convention => C;

   -- /usr/include/sys/socket.h
   -- #define AF_UNSPEC       0               /* unspecified */
   -- #define AF_UNIX         1               /* local to host (pipes) */
   -- #if !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE)
   -- #define AF_LOCAL        AF_UNIX         /* backward compatibility */
   AF_UNSPEC : constant Protocol_Family := 0;

   -- /usr/include/netinet/in.h
   -- #define IPPROTO_IP              0               /* dummy for IP */
   -- #if !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE)
   -- #define IPPROTO_HOPOPTS 0               /* IP6 hop-by-hop options */
   -- #endif  /* (!_POSIX_C_SOURCE || _DARWIN_C_SOURCE) */
   -- #define IPPROTO_ICMP            1               /* control message protocol */
   IPPROTO_IP   : constant Socket_Protocol := 0;
   IPPROTO_ICMP : constant Socket_Protocol := 1;

    -- #define	AI_PASSIVE	0x00000001 /* get address to use bind() */
    -- #define	AI_CANONNAME	0x00000002 /* fill ai_canonname */
    -- #define	AI_NUMERICHOST	0x00000004 /* prevent host name resolution */
    -- #define	AI_NUMERICSERV	0x00001000 /* prevent service name resolution */
   AI_PASSIVE   : constant ai_flags_t := 16#00000001#;
   AI_CANONNAME : constant ai_flags_t := 16#00000002#;

   -- /usr/include/sys/poll.h
   -- #define POLLIN          0x0001          /* any readable data available */
   -- #define POLLPRI         0x0002          /* OOB/Urgent readable data */
   -- #define POLLOUT         0x0004          /* file descriptor is writeable */
   -- #define POLLRDNORM      0x0040          /* non-OOB/URG data available */
   -- #define POLLWRNORM      POLLOUT         /* no write type differentiation */
   -- #define POLLRDBAND      0x0080          /* OOB/Urgent readable data */
   -- #define POLLWRBAND      0x0100          /* OOB/Urgent data can be written */

   -- /*
   --  * FreeBSD extensions: polling on a regular file might return one
   --  * of these events (currently only supported on local filesystems).
   --  */
   -- #define POLLEXTEND      0x0200          /* file may have been extended */
   -- #define POLLATTRIB      0x0400          /* file attributes may have changed */
   -- #define POLLNLINK       0x0800          /* (un)link/rename may have happened */
   -- #define POLLWRITE       0x1000          /* file's contents may have changed */

   -- /*
   --  * These events are set if they occur regardless of whether they were
   --  * requested.
   --  */
   -- #define POLLERR         0x0008          /* some poll error occurred */
   -- #define POLLHUP         0x0010          /* file descriptor was "hung up" */
   -- #define POLLNVAL        0x0020          /* requested events "invalid" */
   --
   -- #define POLLSTANDARD    (POLLIN|POLLPRI|POLLOUT|POLLRDNORM|POLLRDBAND|\
   --                          POLLWRBAND|POLLERR|POLLHUP|POLLNVAL)
   type Poll_Events is mod 2**Interfaces.C.short'size;

   POLLIN     : constant Poll_Events := 16#0001#;
   POLLPRI    : constant Poll_Events := 16#0002#;
   POLLOUT    : constant Poll_Events := 16#0004#;
   POLLRDNORM : constant Poll_Events := 16#0040#;
   POLLWRNORM : constant Poll_Events := POLLOUT;
   POLLRDBAND : constant Poll_Events := 16#0080#;
   POLLWRBAND : constant Poll_Events := 16#0100#;

   POLLEXTEND : constant Poll_Events := 16#0200#;
   POLLATTRIB : constant Poll_Events := 16#0400#;
   POLLNLINK  : constant Poll_Events := 16#0800#;
   POLLWRITE  : constant Poll_Events := 16#1000#;

   POLLERR    : constant Poll_Events := 16#0008#;
   POLLHUP    : constant Poll_Events := 16#0010#;
   POLLNVAL   : constant Poll_Events := 16#0020#;

   -- use type Poll_Events;
   POLLSTANDARD : constant Poll_Events := POLLIN or POLLPRI or POLLOUT
      or POLLRDNORM or POLLRDBAND or POLLWRBAND or POLLERR or POLLHUP
      or POLLNVAL;

end Networking.Types;