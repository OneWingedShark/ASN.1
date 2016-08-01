Package Body ASN_1.Types.OID with Pure, SPARK_Mode => On is

    -- X.690 8.1.2.4.1
    Function Valid( Input : High_Octets ) return Boolean is
	First_Index : Constant Positive := Input'First;
	Last_Index  : Constant Natural  := Input'Last;
	Last_Item   : Subsequent_Octet renames Input(Last_Index);
	Check_Last  : constant Boolean := Last_Item.Continue = False;
	subtype Internal is Natural range First_Index..Natural'Pred( Last_Index );

	Check_Internal : Constant Boolean :=
	  (for all E of Input(Internal) => E.Continue);
    Begin
	return Check_Internal and Check_Last;
    End Valid;

    Function Valid( Input : OID ) return Boolean is
    Begin
	Return (if Input.Length /= 0 then
                   Input.Tag_Number = 2#11111# and then Valid(Input.Subsequent)
	    );
    End Valid;



End ASN_1.Types.OID;
