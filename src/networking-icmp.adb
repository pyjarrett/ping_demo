with Ada.Text_IO;
with Interfaces;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

with Networking.Sockets;  use Networking.Sockets;

package body Networking.ICMP is
    pragma Assertion_Policy(Check);

    package TIO renames Ada.Text_IO;

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

    type Echo_Request_Data_Size is new Integer range 0 .. 1024;
    type Echo_Request_Data is array (Echo_Request_Data_Size range <>) of Interfaces.Unsigned_8
        with Convention => C;

    -- [ICMP](https://datatracker.ietf.org/doc/html/rfc792)
    type Echo_Request_Header is record
        Request_Type : Interfaces.Integer_8 := 8; -- Echo Reply
        Code         : Interfaces.Integer_8 := 8;

	    -- "The checksum is the 16-bit one's complement of the one's
	    -- complement sum of the ICMP message starting with the ICMP Type."
	    -- - RFC 792
        Checksum     : Interfaces.Unsigned_16 := 0;
        Identifier   : Interfaces.Unsigned_16 := 0;
        Sequence_Num : Interfaces.Unsigned_16 := 0;
    end record
        with Convention => C;

    procedure Test_Sizes is
        Empty : Echo_Request_Header;
    begin
        -- static_assert(sizeof(EchoRequest) == 8, "ICMPv4 packet is incorrect size.");
        TIO.Put_Line (Interfaces.Integer_64'Image (Empty'Size));
        pragma Assert (Empty'Size = 64);
    end Test_Sizes;

    -- Pings a host, reporting status to the user.
    procedure Ping(Host : String)
    is
        Host_CStr : aliased Interfaces.C.char_array := Interfaces.C.To_C (host);
        Hints     : constant addrinfo := Make_Hint_ICMP_V4;

        use type Interfaces.C.Strings.chars_ptr;
        package Addrinfo_Conversions is new System.Address_To_Access_Conversions (Object => addrinfo);

        Address_Infos : Addrinfo_Conversions.Object_Pointer := null;
        Success : constant := 0;
    begin
        Test_Sizes;

        -- Gets the address to look up.
        if getaddrinfo (
            Interfaces.C.Strings.To_Chars_Ptr (Host_CStr'Unchecked_Access),
            Interfaces.C.Strings.Null_Ptr,
            System.Storage_Elements.To_Integer (Hints'Address),
            Address_Infos'Address) /= Success
        then
            -- TODO: Write to STDERR
            TIO.Put_Line ("Unable to find address to look up");
            return;
        end if;

        if Address_Infos.ai_canonname = Interfaces.C.Strings.Null_Ptr then
            TIO.Put_Line ("Null canonical name string");
        end if;

        declare
            Client_Socket : Socket_Descriptor := socket (
                int(Address_Infos.ai_family),
                Address_Infos.ai_socktype,
                Address_Infos.ai_protocol
            );
            Connect_Result : Connect_Status;
        begin
            if Client_Socket = Invalid_Socket then
                Print_Error ("Unable to create socket.");
                Print_Error (Get_Errno_String);
                return;
            else
                TIO.Put_Line ("Created the send socket.");
            end if;

            Connect_Result := connect (Client_Socket, Address_Infos.ai_addr, Address_Infos.ai_addrlen);
            if Connect_Result /= Connect_Success then
                TIO.Put_Line ("Unable to connect to socket:" & Connect_Status'Image (Connect_Result));
                TIO.Put_Line ("Socket Error: " & Get_Errno_String);
                close (Client_Socket);
                Client_Socket := Invalid_Socket;
                return;
            end if;

            declare
                use System.Storage_Elements;
                use type System.Storage_Elements.Storage_Offset;
                use type System.Storage_Elements.Integer_Address;
                use type System.Address;

                Send_Buffer_Size : constant System.Storage_Elements.Storage_Offset := 2048;
                Send_Buffer : System.Storage_Elements.Storage_Array (1 .. Send_Buffer_Size);
                Echo_Request : Echo_Request_Header with Import;
                for Echo_Request'Address use Send_Buffer'Address;

                Echo_Request_Payload : Echo_Request_Data(1 .. 1024) with Import;
                for Echo_Request_Payload'Address use Send_Buffer'Address + Echo_Request'Size;

                pragma Assert(not Echo_Request'Overlaps_Storage(Echo_Request_Payload));
                pragma Assert(Echo_Request'Address + Echo_Request'Size = Echo_Request_Payload'Address);
                Request     : Echo_Request_Header;
                Data        : constant Void_Ptr := Send_Buffer'Address;
                Data_Length : constant Interfaces.C.size_t := 0;
                Flags       : constant int := 0;
                Send_Result : constant Send_Status := Send_Error;
            begin
                -- Set the data

                -- Set the checksum

                -- Send_Result := send (Client_Socket, Data, Data_Length, Flags);
                -- if Send_Result = Send_Error then
                --     null;
                    Print_Error ("Unable to send.");
                    Print_Error ("Are you sending as administrator?");
                -- else
                --     TIO.Put_Line ("Wrote bytes: " & Send_Status'Image (Send_Result));
                -- end if;
            end;
        end;



    --     EchoRequest echoRequest(1, 1);
    -- #if _WIN32
    --         closesocket(clientSocket);
    -- #elif __APPLE__
    --         close(clientSocket);
    -- #endif
    --         return false;
    --     }
    --     logInfo() << "Sent: " << sendResult << " bytes\n";

    --     std::vector<char> buffer;
    --     buffer.resize(1024);
    --     int recvResult = recv(clientSocket, buffer.data(), (int)buffer.size(), 0);
    --     if (recvResult > 0) {
    --         logInfo() << "Received " << recvResult << " bytes\n";
    --     } else if (recvResult == 0) {
    --         logInfo() << "Socket closed\n";
    --     } else {
    -- #if _WIN32
    --         logError() << "Receive failed: " << WSAGetLastError() << '\n';
    -- #elif __APPLE__
    --         logError() << "Receive failed: " << errno << '\n';
    -- #endif
        -- }

        TIO.Put_Line ("Pinging: " & Host);
        TIO.Put_Line (Integer'Image(Hints'Size));
    end Ping;

end Networking.ICMP;
