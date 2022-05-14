with Networking;

with Trendy_Test.Assertions.Integer_Assertions;
with Trendy_Test.Assertions.Discrete;

with Ada.Text_IO;

package body Networking.Tests is

   package U16_Assertions is new Trendy_Test.Assertions.Discrete(U16);
   use U16_Assertions;
   use type Networking.U16;

   procedure Checksum_Test (Op : in out Trendy_Test.Operation'Class) is
   begin
      Op.Register;

      declare
         Empty_Array : Networking.U8_Buffer (1 .. 0);
      begin
         Assert_EQ (Op, not U16 (0), Networking.Calculate_Checksum (Empty_Array));
      end;

      declare
         RFC1071_Example : constant Networking.U8_Buffer := (
            16#00#, 16#01#,
            16#f2#, 16#03#,
            16#f4#, 16#f5#,
            16#f6#, 16#f7#
         );
      begin
         Assert_EQ (Op, not U16 (16#ddf2#), Networking.Calculate_Checksum (RFC1071_Example));
      end;

      declare
         Empty_Packet : constant Networking.U8_Buffer := (
            16#08#, 16#00#, -- type, code
            16#00#, 16#00#, -- checksum
            16#00#, 16#01#, -- ID
            16#00#, 16#15#  -- sequence number
         );
      begin
         Assert_EQ (Op, U16 (16#f7e9#), Networking.Calculate_Checksum (Empty_Packet));
      end;

      declare
         Bad_Checksum : constant Networking.U8_Buffer := (
            16#08#, 16#00#, -- type, code
            16#00#, 16#00#, -- checksum
            16#00#, 16#01#, -- ID
            16#00#, 16#18#, -- sequence number
            16#61#, 16#62#  -- data
         );
      begin
         Ada.Text_IO.Put_Line (U16'Image (Networking.Calculate_Checksum (Bad_Checksum)));
         Assert_EQ (Op, U16 (16#9686#), Networking.Calculate_Checksum (Bad_Checksum));
      end;
      
   end Checksum_Test;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return (1 => Checksum_Test'Access);
   end All_Tests;

end Networking.Tests;