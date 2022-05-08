with Ada.Text_IO;

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

      -- TODO: Storing in 
      Sum := Interfaces.Shift_Left(Sum and 16#FF#, 8) or Interfaces.Shift_Right(Sum and 16#FF00#, 8); 

      return not U16 (Sum);
   end Calculate_Checksum;
   
    procedure Print_Bytes (Address : System.Address; Num_Bytes : Natural) is
        type U8_Array is array (Positive range 1 .. Num_Bytes) of Interfaces.Unsigned_8;
        Bytes : constant U8_Array with Import;
        for Bytes'Address use Address;
    begin
        for Byte of Bytes loop
            Ada.Text_IO.Put_Line (Byte'Image);
        end loop;
    end Print_Bytes;

end Networking;