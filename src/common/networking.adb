with Ada.Text_IO;
with Interfaces;

package body Networking is

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
      use type U16;
      use type U32;

      -- Create a byte array to overlay on the given storage, used as a buffer.
      -- Handle the primary case of summing each group of two bytes by treating
      -- the array as if it were an entire unsigned 16.
      type U16_Array is array (Positive range <>) of U16;

      -- The length here is halved.  Any extra odd byte will be added separately.
      -- "Import" tells the compiler not to reserve space for it.
      Pairs : U16_Array (1 .. Buffer'Length / 2) with Import;
      for Pairs'Address use Buffer'Address;

      Sum : U32 := 0;
   begin
      -- Sum the rest of the bytes pairs.
      for Value of Pairs loop
         Sum := Sum + U32 (Value);
      end loop;
      
      -- If the number of bytes in the buffer is odd, start the sum using that last
      -- byte.  This works because summing is communitive.
      --
      -- Assume little-endian, so little byte comes first, so it can be added
      -- by itself without a shift.
      if Buffer'Length mod 2 = 1 then
         Sum := Sum + U32 (Buffer (Buffer'Last));
      end if;

      -- Wrap around the higher-order bits and add them back into the checksum.
      while Interfaces.Shift_Right (Sum, 16) > 0 loop
         Sum := (Sum and 16#FFFF#) + Interfaces.Shift_Right (Sum, 16);
      end loop;

      -- TODO: Storing in 
      -- Sum := Interfaces.Shift_Left(Sum and 16#FF#, 8) or (Interfaces.Shift_Right(Sum and 16#FF00#, 8) and 16#FF#);

      return not U16 (Sum and 16#FFFF#);
   end Calculate_Checksum;

   function Swap_Endianness (U : U16) return U16
   is 
      use type U16;
   begin
      return (Interfaces.Shift_Right (U, 8) or Interfaces.Shift_Left (U and 16#FF#, 8));
   end Swap_Endianness;

   procedure Print_Bytes (Address : System.Address; Num_Bytes : Natural) is
      type U8_Array is array (Positive range 1 .. Num_Bytes) of Interfaces.Unsigned_8;
      Bytes : constant U8_Array with Import;
      for Bytes'Address use Address;
      package BIO is new Ada.Text_IO.Modular_IO (Interfaces.Unsigned_8);
   begin
      for Byte of Bytes loop
         BIO.Put (Item => Byte, Base => 16);
         Ada.Text_IO.New_Line;
      end loop;
   end Print_Bytes;

   -- Returns the padded hexadecimal representation of a U16, with the
   -- high end bytes printed first.
   --
   --   Byte 2      Byte 1
   -- 0000 0000   0000 0000
   --
   -- Each nibble is a character and requires a number of shifts to the right
   -- 3, 2, 1, 0
   -- 
   -- Indexing into a string gives us the character.
   function As_Hex (U : U16) return String
   is
      use type U16;
      subtype Nibble_Index is Positive range 1 .. (U16'Size / 4);
      subtype Nibble is U16 range 0 .. 15;

      function Get_Nibble (U : U16; Index : Nibble_Index) return Nibble
         is (Nibble (Interfaces.Shift_Right (U, Natural ((Nibble_Index'Last - Index) * 4)) and 16#F#))         
         with Inline;

      pragma Assert (Get_Nibble (16#abcd#, 1) = 16#a#);
      pragma Assert (Get_Nibble (16#abcd#, 2) = 16#b#);
      pragma Assert (Get_Nibble (16#abcd#, 3) = 16#c#);
      pragma Assert (Get_Nibble (16#abcd#, 4) = 16#d#);

      Chars : constant array (Nibble) of Character := "0123456789abcdef";
   begin
      return Result : String (1 .. Positive (Nibble_Index'Last)) := (others => '0') do
         for Index in Nibble_Index'Range loop
            Result (Index) := Chars (Get_Nibble (U, Index));
         end loop;
      end return;
   end As_Hex;

   function As_Hex (U : U32) return String
   is
      use type U32;
      subtype Nibble_Index is Positive range 1 .. (U32'Size / 4);
      subtype Nibble is U32 range 0 .. 15;

      function Get_Nibble (U : U32; Index : Nibble_Index) return Nibble
         is (Nibble (Interfaces.Shift_Right (U, Natural ((Nibble_Index'Last - Index) * 4)) and 16#F#))
         with Inline;

      Result : String (1 .. Integer (Nibble_Index'Last)) := (others => '0');
      Chars : constant array (Nibble) of Character := "0123456789abcdef";
   begin
      for Index in Nibble_Index'Range loop
         Result (Index) := Chars (Get_Nibble (U, Index));
      end loop;
      return Result;
   end As_Hex;

end Networking;