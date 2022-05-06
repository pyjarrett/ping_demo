with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body Networking is

   function Get_Errno_String return String
   is
      -- __BEGIN_DECLS
      -- extern int * __error(void);
      -- #define errno (*__error())
      -- __END_DECLS
      function errno return System.Address
         with Import, Convention => C, External_Name => "__error";

      --  char* strerror(int errnum);
      function strerror (errnum : int) return Interfaces.C.Strings.chars_ptr
         with Import, Convention => C;

      package Int_Conversions is new System.Address_To_Access_Conversions(int);
      Errno_Address : constant System.Address := errno;
      Errno_Ptr     : constant access int := Int_Conversions.To_Pointer (Errno_Address);
      Errno_Str     : constant String := Interfaces.C.Strings.Value (strerror (Errno_Ptr.all));
   begin
      return Errno_Str;
   end Get_Errno_String;

   -- The primary use case for checksum calculation will be the calculation of
   -- a candidate packet inside a buffer.
   --
   -- Calculates a checksum according to RFC 1071:
   -- https://datatracker.ietf.org/doc/html/rfc1071
   function Calculate_Checksum (Buffer : System.Storage_Elements.Storage_Array)
      return U16
   is
      As_U8_Buffer : U8_Buffer (1 .. Buffer'Length) with Import;
      for As_U8_Buffer'Address use Buffer'Address;
   begin
      return Calculate_Checksum (As_U8_Buffer);
   end Calculate_Checksum;

   -- The primary use case for checksum calculation will be the calculation of
   -- a candidate packet inside a buffer.
   --
   -- Calculates a checksum according to RFC 1071:
   -- https://datatracker.ietf.org/doc/html/rfc1071
   function Calculate_Checksum (Buffer : U8_Buffer)
      return U16
   is
      -- Create a byte array to overlay on the given storage, used as a buffer.
      -- Handle the primary case of summing each group of two bytes by treating
      -- the array as if it were an entire unsigned 16.
      type U16_Array is array (Positive range <>) of U16;

      -- The length here is halved.  Any extra odd byte will be added separately.
      -- "Import" tells the compiler not to reserve space for it.
      Pairs : U16_Array (1 .. Buffer'Length / 2) with Import;
      for Pairs'Address use Buffer'Address;

      -- Provide a conversion for the last odd byte length.
      use type U16;
      use type U32;
      -- use type U64;
      Sum : U32 := 0;
   begin
      -- If the number of bytes in the buffer is odd, start the sum using that last
      -- byte.  This works because summing is communitive.
      --
      -- Assume little-endian, so little byte comes first, so it can be added
      -- by itself without a shift.
      if Pairs'Length mod 2 = 1 then
         Sum := Sum + U32 (Buffer (Buffer'Last));
      end if;

      -- Sum the rest of the bytes pairs.
      for Value of Pairs loop
         Sum := Sum + U32 (Value);
      end loop;

      -- Wrap around the higher-order bits and add them back into the checksum.
      while Interfaces.Shift_Right (Sum, 16) > 0 loop
         Sum := (Sum and 16#FFFF#) + Interfaces.Shift_Right (Sum, 16);
      end loop;

      return not U16 (Sum);
   end Calculate_Checksum;

end Networking;