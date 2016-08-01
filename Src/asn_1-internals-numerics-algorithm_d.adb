Separate(ASN_1.Internals.Numerics)

--  The complex full multi-precision case. We will employ algorithm
--  D defined in the section "The Classical Algorithms" (sec. 4.3.1)
--  of Donald Knuth's "The Art of Computer Programming", Vol. 2, 2nd
--  edition. The terminology is adjusted for this section to match that
--  reference.
--
--  We are dividing X.Len digits of X (called u here) by Y.Len digits
--  of Y (called v here), developing the quotient and remainder. The
--  numbers are represented using Base, which was chosen so that we have
--  the operations of multiplying to single digits (SD) to form a double
--  digit (DD), and dividing a double digit (DD) by a single digit (SD)
--  to give a single digit quotient and a single digit remainder.
--
--  Algorithm D from Knuth
--
--  Comments here with square brackets are directly from Knuth
Function Algorithm_D(X,Y : Big_Number;
		     Discard_Quotient  : Boolean := False;
		     Discard_Remainder : Boolean := False
		    ) return Quotient_Remainder is
    --  The following lower case variables correspond exactly to the
    --  terminology used in algorithm D.

    m : constant Length_Range := X.Length - Y.Length;
    n : constant Length_Range := Y.Length;
    b : constant Double_Digit := Base;

    u : Digit_Vector (0 .. m + n);
    v : Digit_Vector (1 .. n);
    q : Digit_Vector (0 .. m);
    r : Digit_Vector (1 .. n);

    u0 : Digit renames u (0);
    v1 : Digit renames v (1);
    v2 : Digit renames v (2);

    d    : Double_Digit;
    j    : Length_Range;
    qhat : Double_Digit;
    rhat : Double_Digit;
    temp : Double_Digit;

begin
    --  Initialize data of left and right operands

    for J in 1 .. m + n loop
	u (J) := X.Data (J);
    end loop;

    for J in 1 .. n loop
	v (J) := Y.Data (J);
    end loop;

    --  [Division of nonnegative integers.] Given nonnegative integers u
    --  = (ul,u2..um+n) and v = (v1,v2..vn), where v1 /= 0 and n > 1, we
    --  form the quotient u / v = (q0,ql..qm) and the remainder u mod v =
    --  (r1,r2..rn).

    pragma Assert (v1 /= 0);
    pragma Assert (n > 1);

    --  Dl. [Normalize.] Set d = b/(vl + 1). Then set (u0,u1,u2..um+n)
    --  equal to (u1,u2..um+n) times d, and set (v1,v2..vn) equal to
    --  (v1,v2..vn) times d. Note the introduction of a new digit position
    --  u0 at the left of u1; if d = 1 all we need to do in this step is
    --  to set u0 = 0.

    d := b / (Double_Digit(v1) + 1);

    if d = 1 then
	u0 := 0;

    else
	declare
	    Carry : Double_Digit;
	    Tmp   : Double_Digit;

	begin
	    --  Multiply Dividend (u) by d

	    Carry := 0;
	    for J in reverse 1 .. m + n loop
		Tmp   := Double_Digit (u (J)) * d + Carry;
		u (J) := LSD (Tmp);
		Carry := Tmp / Base;
	    end loop;

	    u0 := Digit (Carry);

	    --  Multiply Divisor (v) by d

	    Carry := 0;
	    for J in reverse 1 .. n loop
		Tmp   := Double_Digit (v (J)) * d + Carry;
		v (J) := LSD (Tmp);
		Carry := Tmp / Base;
	    end loop;

	    pragma Assert (Carry = 0);
	end;
    end if;

    --  D2. [Initialize j.] Set j = 0. The loop on j, steps D2 through D7,
    --  will be essentially a division of (uj, uj+1..uj+n) by (v1,v2..vn)
    --  to get a single quotient digit qj.

    j := 0;

    --  Loop through digits

    loop
	--  Note: In the original printing, step D3 was as follows:

	--  D3. [Calculate qhat.] If uj = v1, set qhat to b-l; otherwise
	--  set qhat to (uj,uj+1)/v1. Now test if v2 * qhat is greater than
	--  (uj*b + uj+1 - qhat*v1)*b + uj+2. If so, decrease qhat by 1 and
	--  repeat this test

	--  This had a bug not discovered till 1995, see Vol 2 errata:
	--  http://www-cs-faculty.stanford.edu/~uno/err2-2e.ps.gz. Under
	--  rare circumstances the expression in the test could overflow.
	--  This version was further corrected in 2005, see Vol 2 errata:
	--  http://www-cs-faculty.stanford.edu/~uno/all2-pre.ps.gz.
	--  The code below is the fixed version of this step.

	--  D3. [Calculate qhat.] Set qhat to (uj,uj+1)/v1 and rhat to
	--  to (uj,uj+1) mod v1.

	temp := u (j) & u (j + 1);
	qhat := temp / Double_Digit (v1);
	rhat := temp mod Double_Digit (v1);

	--  D3 (continued). Now test if qhat >= b or v2*qhat > (rhat,uj+2):
	--  if so, decrease qhat by 1, increase rhat by v1, and repeat this
	--  test if rhat < b. [The test on v2 determines at high speed
	--  most of the cases in which the trial value qhat is one too
	--  large, and eliminates all cases where qhat is two too large.]

	while qhat >= b
	  or else Double_Digit (v2) * qhat > LSD (rhat) & u (j + 2)
	loop
	    qhat := qhat - 1;
	    rhat := rhat + Double_Digit (v1);
	    exit when rhat >= b;
	end loop;

	--  D4. [Multiply and subtract.] Replace (uj,uj+1..uj+n) by
	--  (uj,uj+1..uj+n) minus qhat times (v1,v2..vn). This step
	--  consists of a simple multiplication by a one-place number,
	--  combined with a subtraction.

	--  The digits (uj,uj+1..uj+n) are always kept positive; if the
	--  result of this step is actually negative then (uj,uj+1..uj+n)
	--  is left as the true value plus b**(n+1), i.e. as the b's
	--  complement of the true value, and a "borrow" to the left is
	--  remembered.

	declare
	    Borrow : Digit;
	    Carry  : Double_Digit;
	    Temp   : Double_Digit;

	    Negative : Boolean;
	    --  Records if subtraction causes a negative result, requiring
	    --  an add back (case where qhat turned out to be 1 too large).

	begin
	    Borrow := 0;
	    for K in reverse 1 .. n loop
		Temp := qhat * Double_Digit (v (K)) + Double_Digit (Borrow);
		Borrow := MSD (Temp);

		if LSD (Temp) > u (j + K) then
		    Borrow := Borrow + 1;
		end if;

		u (j + K) := u (j + K) - LSD (Temp);
	    end loop;

	    Negative := u (j) < Borrow;
	    u (j) := u (j) - Borrow;

	    --  D5. [Test remainder.] Set qj = qhat. If the result of step
	    --  D4 was negative, we will do the add back step (step D6).

	    q (j) := LSD (qhat);

	    if Negative then

		--  D6. [Add back.] Decrease qj by 1, and add (0,v1,v2..vn)
		--  to (uj,uj+1,uj+2..uj+n). (A carry will occur to the left
		--  of uj, and it is be ignored since it cancels with the
		--  borrow that occurred in D4.)

		q (j) := q (j) - 1;

		Carry := 0;
		for K in reverse 1 .. n loop
		    Temp := Double_Digit(v (K)) + Double_Digit(u (j + K)) + Carry;
		    u (j + K) := LSD (Temp);
		    Carry := Temp / Base;
		end loop;

		u (j) := u (j) + Digit(Carry);
	    end if;
	end;

	--  D7. [Loop on j.] Increase j by one. Now if j <= m, go back to
	--  D3 (the start of the loop on j).

	j := j + 1;
	exit when not (j <= m);
    end loop;

    --  D8. [Unnormalize.] Now (qo,ql..qm) is the desired quotient, and
    --  the desired remainder may be obtained by dividing (um+1..um+n)
    --  by d.

    declare
	Function Get_Remainder return Big_Number is
	    Remdr : Double_Digit;

	begin
	    Remdr := 0;
	    for K in 1 .. n loop
		Remdr := Base * Remdr + Double_Digit(u (m + K));
		r (K) := Digit(Remdr / d);
		Remdr := Remdr rem d;
	    end loop;

	    pragma Assert (Remdr = 0);

	    Return Normalize (r);
	end Get_Remainder;

	Quotient : Constant Big_Number :=
	  (if not Discard_Quotient then Normalize (q) else Zero);
	Remainder : Constant Big_Number :=
	  (if not Discard_Remainder then Get_Remainder else Zero);

    begin
	Return ( Quotient_Length  => Quotient.Length,
	  Remainder_Length => Remainder.Length,
	  Quotient         => Quotient,
	  Remainder        => Remainder
	 );
    end;


End Algorithm_D;
