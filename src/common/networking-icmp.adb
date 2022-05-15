with Ada.Text_IO;
with Interfaces;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

with Networking.Error;
with Networking.Sockets;  use Networking.Sockets;
with Networking.Types;    use Networking.Types;

package body Networking.ICMP is
   pragma Assertion_Policy(Check);

   package TIO renames Ada.Text_IO;

   -- Simplify error printing.
   procedure Print_Error (S : String) is
   begin
      TIO.Put_Line (TIO.Standard_Error, S);
   end Print_Error;

   function Make_Hint_ICMP_V4 return addrinfo is
   begin
      return (
         ai_family   => AF_UNSPEC,
         ai_socktype => SOCK_RAW,
         ai_protocol => IPPROTO_ICMP, -- or IPPROTO_ICMPV6
         others      => <>
      );
   end Make_Hint_ICMP_V4;

   -- [ICMP](https://datatracker.ietf.org/doc/html/rfc792)
   type Echo_Request_Header is record
      Request_Type : Interfaces.Unsigned_8 := 8; -- Echo Reply
      Code         : Interfaces.Unsigned_8 := 0;

      -- "The checksum is the 16-bit one's complement of the one's
      -- complement sum of the ICMP message starting with the ICMP Type."
      -- - RFC 792
      Checksum     : Interfaces.Unsigned_16 := 0;
      Identifier   : Interfaces.Unsigned_16 := 0;
      Sequence_Num : Interfaces.Unsigned_16 := 0;
   end record;

   for Echo_Request_Header use record
      Request_Type at 0 range 0 .. 7;
      Code         at 1 range 0 .. 7;
      Checksum     at 2 range 0 .. 15;
      Identifier   at 4 range 0 .. 15;
      Sequence_Num at 6 range 0 .. 15;
   end record;
   for Echo_Request_Header'Bit_Order use System.High_Order_First;
   for Echo_Request_Header'Scalar_Storage_Order use System.High_Order_First;

   procedure Test_Sizes is
      Empty : Echo_Request_Header;
   begin
      -- static_assert(sizeof(EchoRequest) == 8, "ICMPv4 packet is incorrect size.");
      TIO.Put_Line (Interfaces.Integer_64'Image (Empty'Size));
      pragma Assert (Empty'Size = 64);
   end Test_Sizes;

   --
   --
   procedure Send_Ping (
      Client_Socket : Socket_Descriptor;
      Payload       : String
   ) is
      use type System.Storage_Elements.Storage_Offset;
      use type System.Address;
      use type int;


      -- Underlying buffer for the send.
      pragma Warnings(Off, "overlay changes scalar storage order");
      Buffer_Size : constant System.Storage_Elements.Storage_Offset :=
         Echo_Request_Header'Size / 8 + Payload'Length;
      Buffer      : System.Storage_Elements.Storage_Array (1 .. Buffer_Size);

      -- Map request and payload onto the buffer.
      Echo_Request     : Echo_Request_Header with Import;
      for Echo_Request'Address use Buffer'Address;
      pragma Warnings(On, "overlay changes scalar storage order");

      Echo_Request_Payload : String (1 .. Payload'Length) with Import;
      for Echo_Request_Payload'Address use Buffer'Address + Echo_Request_Header'Size / 8;

      -- Verify the request and payload are where we want.
      pragma Assert(not Echo_Request'Overlaps_Storage(Echo_Request_Payload));
      pragma Assert(Echo_Request'Address + Echo_Request_Header'Size / 8 = Echo_Request_Payload'Address);
      Flags  : constant int := 0;
      Result : Send_Status := Send_Error;
   begin
      -- Build the packet, calculate the checksum and send.
      Echo_Request := (
         Request_Type => 8,
         Code => 0,
         Checksum => 0, 
         Identifier => 1,
         Sequence_Num => 1);
      Echo_Request_Payload := Payload;

      -- Need to swap endianness since we're calculating the checksum in big
      -- endian space and when stored, it's going to be swapped by the structure.
      Echo_Request.Checksum := Swap_Endianness (Networking.Calculate_Checksum (Buffer (1 .. Buffer_Size)));
      TIO.Put_Line ("Checksum: " & Interfaces.Unsigned_16'Image (Echo_Request.Checksum));

      Result := send (Client_Socket, Buffer'Address, Interfaces.C.int (Buffer_Size), Flags);
      if Result = Send_Error then
         Print_Error ("Unable to send.");
         Print_Error ("Are you sending as administrator?");
      else
         TIO.Put_Line ("Wrote bytes: " & Send_Status'Image (Result));
      end if;
   end Send_Ping;

   -- Receives data off of a socket.
   procedure Receive_Ping (
      Socket           : Socket_Descriptor;
      Expected_Payload : String
   ) is
      pragma Unreferenced (Expected_Payload);

      use type System.Storage_Elements.Storage_Offset;
      use type int;

      pragma Warnings(Off, "overlay changes scalar storage order");
      Buffer_Size : constant := 1024;
      Buffer : System.Storage_Elements.Storage_Array (1 .. Buffer_Size);
      IP_Header    : IPv4_Header with Import;
      Echo_Receipt : Echo_Request_Header with Import;
      for IP_Header'Address use Buffer'Address;
      for Echo_Receipt'Address use IP_Header'Address + IP_Header'Size / 8;
      pragma Warnings(On, "overlay changes scalar storage order");
      Result : int;
   begin
      Result := recv (Socket, Buffer'Address, Buffer_Size, 0);
      if Result > 0 then
         declare
            Echo_Payload : String (1 .. 
               Integer (Result) - IP_Header'Size / 8 - Echo_Request_Header'Size / 8) with Import;
            for Echo_Payload'Address use Echo_Receipt'Address + Echo_Request_Header'Size / 8;
         begin
            TIO.Put_Line ("Received: " & Result'Image & " bytes " & Echo_Payload);
         end;
      elsif Result = 0 then
         TIO.Put_Line ("Socket closed");
      else
         TIO.Put_Line ("Socket error: " & Result'Image);
      end if;
   end Receive_Ping;

   function Is_Socket_Ready (S : Socket_Descriptor) return Boolean is
     poll_fds             : pollfd_array (1 .. 1) := (1 => (fd => S, events => POLLIN, revents => 0));
     Timeout_Milliseconds : constant Interfaces.C.int := 2000;
     use type Interfaces.C.int;
   begin
     return poll (poll_fds, 1, Timeout_Milliseconds) > 0;
   end Is_Socket_Ready;
   
   -- Pings a host, reporting status to the user.
   procedure Ping (
      Host    : String;
      Payload : String)
   is
      Host_CStr : aliased Interfaces.C.char_array := Interfaces.C.To_C (host);
      Hints     : constant addrinfo := Make_Hint_ICMP_V4;

      package Addrinfo_Conversions is new System.Address_To_Access_Conversions (Object => addrinfo);

      Address_Infos : Addrinfo_Conversions.Object_Pointer := null;
      Success : constant := 0;

      Client_Socket : Socket_Descriptor;
      Connect_Result : Connect_Status;
      
      use type int;
      use type Interfaces.C.Strings.chars_ptr;
      use type Socket_Descriptor;

   begin
      Test_Sizes;

      -- Gets the address to look up.
      if getaddrinfo (
         Interfaces.C.Strings.To_Chars_Ptr (Host_CStr'Unchecked_Access),
         Interfaces.C.Strings.Null_Ptr,
         System.Storage_Elements.To_Integer (Hints'Address),
         Address_Infos'Address) /= Success
      then
         Print_Error ("Unable to find address to look up");
         Print_Error (Networking.Error.Get_Errno_String);
         return;
      end if;

      Client_Socket := socket (
         Address_Infos.ai_family,
         Address_Infos.ai_socktype,
         Address_Infos.ai_protocol
      );

      if Client_Socket = Invalid_Socket then
         Print_Error ("Unable to create socket.");
         Print_Error (Networking.Error.Get_Errno_String);
         return;
      else
         TIO.Put_Line ("Created the send socket.");
      end if;

      Connect_Result := connect (Client_Socket, Address_Infos.ai_addr, Interfaces.C.int (Address_Infos.ai_addrlen));
      if Connect_Result /= Connect_Success then
         Print_Error ("Unable to connect to socket:" & Connect_Status'Image (Connect_Result));
         Print_Error ("Socket Error: " & Networking.Error.Get_Errno_String);
         close (Client_Socket);
         Client_Socket := Invalid_Socket;
         return;
      end if;

      Send_Ping (Client_Socket, Payload);

      if Is_Socket_Ready (Client_Socket) then
         TIO.Put_Line ("Socket is ready.");
      else
         TIO.Put_Line ("Socket is not ready.");
         Print_Error (Networking.Error.Get_Errno_String);
         return;
      end if;

      Receive_Ping (Client_Socket, Payload);

      TIO.Put_Line ("Pinging: " & Host);
      TIO.Put_Line (Integer'Image(Hints'Size));
   end Ping;

end Networking.ICMP;
