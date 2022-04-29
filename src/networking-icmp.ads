with Interfaces.C.Strings;
with System.Address;
with Ada.Unchecked_Conversion;

package Networking.ICMP is
    
    subtype sockaddr_ptr is System.Address;
    subtype addrinfo_ptr is System.Address;
    type socklen_t is new Interfaces.Unsigned_32;

    type Address_Family is (
        AF_UNSPEC
        AF_INET,
    );

    for Address_Family use (
        AF_UNSPEC := 0,
        AF_INET := 2
    );

    type AddrInfo is record
        AI_Flags     : Interfaces.C.Int := 0;
        AI_Family    : Interfaces.C.Int := 0;
        AI_SockType  : Interfaces.C.Int := 0;
        AI_Protocol  : Interfaces.C.Int := 0;
        AI_AddrLen   : socklen_t := 0;
        AI_CanonName : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
        AI_Addr      : sockaddr_ptr := 0;  -- binary address
        AI_Next      : addrinfo_ptr := 0;  -- next structure in linked list
    end record
        with Convention => C;
    
    type Socket_Type is (
        SOCK_STREAM,
        SOCK_DGRAM,
        SOCK_RAW,
        SOCK_RDM,
        SOCK_SEQPACKET)
    with Size => C.Interfaces.int;

    for Socket_Type use (
        SOCK_STREAM    := 1,
        SOCK_DGRAM     := 2,
        SOCK_RAW       := 3,
        SOCK_RDM       := 4,
        SOCK_SEQPACKET := 5);

    procedure Get_Addresses(S : String) is
    begin
        null;

    end Get_Addresses;

    type AddrInfo_Ptr is new System.Address;
    function To_Integer is new Ada.Unchecked_Conversion(AddrInfo_Ptr, Interfaces.Integer_64);
    function To_AddrPtr is new Ada.Unchecked_Conversion(Interfaces.Integer_64, AddrInfo_Ptr);

    function Next(Ptr : AddrInfo_Ptr) return AddrInfo_Ptr is
    begin
        return To_AddrPtr (To_Integer (Ptr) + AddrInfo_Ptr'Size);
    end Next;

    type Address_Alternatives is new Controlled_Type with record
        Alternatives : AddrInfo_Ptr := To_AddrPtr(0);
        Next         : AddrInfo_Ptr := To_AddrPtr(0);
    end record;

    overriding procedure Finalize(Self : in out Address_Alternatives) is
    begin 
        if Self.Alternatives /= 0 then
            freeaddrinfo(Self.Alternatives);
            Self.Alternatives := 0;
        end if;
    end Finalize;

    procedure Current(Self : in Address_Alternatives; AddrInfo : out AddressInfo) is
        Value : AddrInfo;
        for Value'Address use Self.Current;
    begin
        AddrInfo := Value;
    end Current;
    

/// TODO: Other alternative `addrinfo` should be checked, not just the first one.
class AddressAlternatives
{
public:
	/// clang-tidy doesn't like when I make this static and pass by reference--it thinks
	/// `m_alternatives` never gets initialized.
	[[nodiscard]] bool resolve(const char* target, const addrinfo& hints)
	{
		if (!target) {
			return false;
		}

		// Figure out where the ping should go.  This might return multiple results.
		const int addrinfoResult = getaddrinfo(target, nullptr, &hints, &m_alternatives);
		if (addrinfoResult != 0) {
			logError() << "getaddrinfo failed: " << addrinfoResult << '\n';
			return false;
		}
		m_next = m_alternatives;
		return true;
	}

	[[nodiscard]] addrinfo* current() const noexcept { return m_next; }


end Networking.ICMP;