with Ada.Characters.Latin_1;

package body Networking.Sockets is

   function Image (Self : addrinfo) return String is
      use Ada.Characters.Latin_1;
   begin
      return ai_flags_t'Image (Self.ai_flags) & LF
         & " family:   " & Protocol_Family'Image (Self.ai_family) & LF
         & " socktype: " & Socket_Type'Image (Self.ai_socktype) & LF
         & " protocol: " & Socket_Protocol'Image (Self.ai_protocol) & LF
         & " addrlen:  " & socklen_t'Image (Self.ai_addrlen) & LF
         & " address:  " & Image (Self.ai_addr) & LF
         & " next:     " & Image (Self.ai_next);
   end Image;

   function Image (Ptr : Socket_Address_Conversions.Object_Pointer) return String
   is
      use Ada.Characters.Latin_1;
      Data : constant sa_data_t := Ptr.sa_data;
   begin
      return Interfaces.Unsigned_8'Image (Ptr.sa_len) & LF
         & Interfaces.Unsigned_8'Image (Ptr.sa_family) & LF
         & Data (0)'Image
         & Data (1)'Image
         & Data (2)'Image
         & Data (3)'Image
         & Data (4)'Image
         & Data (5)'Image
         & Data (6)'Image
         & Data (7)'Image
         & Data (8)'Image
         & Data (9)'Image
         & Data (10)'Image
         & Data (11)'Image
         & Data (12)'Image
         & Data (13)'Image;
   end Image;

   -- Hide the return value of close() when we don't need it.
   procedure close (File_Descriptor : int) is
      Unused : int;
   begin
      pragma Unreferenced (Unused);
      Unused := close (File_Descriptor);
   end close;

end Networking.Sockets;
