Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
ASN_1.Enumerations.Tags;

Use
ASN_1.Enumerations.Tags;

Package ASN_1.Types.OID with Pure, SPARK_Mode => On is

    Type OID(<>) is private;

--      Function Create

Private
    Type High_Octets;


    Function Valid( Input : OID         ) return Boolean;
    Function Valid( Input : High_Octets ) return Boolean;

    Type Subsequent_Octet is record
	Continue : Boolean;
	Data     : Bits_7;
    end record;

    Type High_Octets is array (Positive Range <>) of Subsequent_Octet;

    Type OID( Atomic : Atomicity; Length : Natural ) is record
	Class        : Tag_Class;
	Tag_Number   : Bits_5;
	case Length is
	when 0      => Null;
	when others => Subsequent : High_Octets( 1..Length );
	end case;
    end record
    with Type_Invariant => Valid(OID);


    For OID use record
	Class      At 0 range 6..7;
	Tag_Number At 0 range 0..4;
	Atomic     At 0 range 5..5;
    end record;

    For Subsequent_Octet use record
	Continue   At 0 range 7..7;
	Data       At 0 range 0..6;
    end record;

End ASN_1.Types.OID;
