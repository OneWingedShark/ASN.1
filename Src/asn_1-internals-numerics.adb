Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
Interfaces,
System;

Package Body ASN_1.Internals.Numerics with Pure, SPARK_Mode => On is

    --  Double digit is used for intermediate computations.
    Type Double_Digit is mod Base**2;

    --  Indicates result of comparison.
    type Compare_Result is (Less_Than, Equal, Greater_Than);



    -- Most significant and least significant digit of double digit value.
    function MSD (X : Double_Digit) return Digit is (Digit(X / Base));
    function LSD (X : Double_Digit) return Digit is (Digit(X mod Base));


    -- Create a double digit value from two single digit values.
    function "&" (X, Y : Digit) return Double_Digit is
      (Double_Digit(X) * Base + Double_Digit(Y));


    function Normalize
     (X   : Digit_Vector;
      Neg : Boolean := False) return Big_Number;

    function Compare
      (X, Y         : Digit_Vector;
       X_Neg, Y_Neg : Boolean) return Compare_Result;

    procedure Div_Rem
      (X, Y              : Big_Number;
       Quotient          : out Big_Number;
       Remainder         : out Big_Number;
       Discard_Quotient  : Boolean := False;
       Discard_Remainder : Boolean := False);

    function Add
      (X, Y  : Digit_Vector;
       X_Neg : Boolean;
       Y_Neg : Boolean) return Big_Number;

--      function "+"   (X, Y : Big_Number) return Big_Number;
--      function "-"   (X, Y : Big_Number) return Big_Number;
--      function "*"   (X, Y : Big_Number) return Big_Number;
--      function "/"   (X, Y : Big_Number) return Big_Number;
--      function "**"  (X, Y : Big_Number) return Big_Number;
--      function "MOD" (X, Y : Big_Number) return Big_Number;
--      function "REM" (X, Y : Big_Number) return Big_Number;
--      function "-"   (X    : Big_Number) return Big_Number;
--      function "ABS" (X    : Big_Number) return Big_Number;

    function Is_In_LLI_Range (X : Big_Number) return Boolean is
      (case X.Length is
	   when 0 | 1  => True,
	   when 2      => (X.Data(1) & X.Data(2)) < 2**63 or else
                          (X.Negetive and then (X.Data(1) & X.Data(2)) = 2**63),
	   when Others => False
      );

    function To_Big_Number (X : Long_Long_Integer) return Big_Number is
      (if X = 0 then Zero                               -- Zero.
       elsif X in -(2 ** 32 - 1) .. +(2 ** 32 - 1) then -- One word results.
	   (Length => 1, Negetive => X < 0, Data => (1 => Digit(abs (X))))
       elsif X = Long_Long_Integer'First then           -- Most negative number.
	 (Length => 2, Negetive => True, Data => (1 => 2**31, 2 => 0))
       else                                             -- Normal two word case.
	 (Length => 2, Negetive => X < 0,
            Data =>(2 => Digit(Abs(X) mod Base), 1 => Digit(Abs X / Base))
         )
      );


    function From_Big_Number (X : Big_Number) return Long_Long_Integer is
      (if not Is_In_LLI_Range(X) then
	     raise Constraint_Error with "expression value out of range"
       else
	   (case X.Length is
            when 0 => 0,
            when 1 => Long_Long_Integer( X.Data(1) * (if X.Negetive then -1 else 1) ),
            when 2 => Long_Long_Integer((X.Data(1) & X.Data(2)) * (if X.Negetive then -1 else 1)),
            when others => raise Program_Error with "Unreachable"
           )
      ) with SPARK_Mode => Off;



    function Normalize
      (X   : Digit_Vector;
       Neg : Boolean := False) return Big_Number with SPARK_Mode => Off is
    begin
	if X'Length = 0 then
	    Return Zero;
	else
	    declare
		First_Non_Zero : Length_Range := X'First;
		Function Past_Bounds return Boolean is
		  ( First_Non_Zero > X'Last ) with Inline;
	    begin
		Find_Leading_Zeros:
		loop
		    exit when X(First_Non_Zero) /= 0;
		    First_Non_Zero:= Length_Range'Succ( First_Non_Zero );
		    exit when Past_Bounds;
		End Loop Find_Leading_Zeros;

		Generate_Result:
		declare
		    Subtype Significant_Range is Length_Range range First_Non_Zero..X'Last;
		    Significant_Portion : Digit_Vector := X(Significant_Range);
		begin
		    Return (if Past_Bounds then Zero
	                    else (Length   => Significant_Portion'Length,
			          Data     => Significant_Portion,
			          Negetive => Neg
                                 ) -- Result aggrigate
	                   );
		End Generate_Result;
	    end;
	end if;
    End Normalize;

    function Compare
      (X, Y         : Digit_Vector;
       X_Neg, Y_Neg : Boolean) return Compare_Result is
    begin
	if X_Neg /= Y_Neg then
	    return (if X_Neg then Less_Than else Greater_Than);
	elsif X'Length /= Y'Length then
	    return (if (X'Last > Y'Last) xor X_Neg then Greater_Than else Less_Than);
	else
	    Return Result : Compare_Result := Equal do
		Test_Elements:
		For Index in X'Range loop
		    if X(Index) /= Y(Index) then
			Result:= (if (X(Index) > Y (Index)) xor X_Neg
                                  then Greater_Than else Less_Than);
			exit Test_Elements;
		    end if;
		End Loop Test_Elements;
	    End return;
	end if;
    End Compare;

    procedure Div_Rem
      (X, Y              : Big_Number;
       Quotient          : out Big_Number;
       Remainder         : out Big_Number;
       Discard_Quotient  : Boolean := False;
       Discard_Remainder : Boolean := False) with SPARK_Mode => Off is

	Subtype LLI is Long_Long_Integer;
	Subtype SD  is Digit;
    begin
      --  Error if division by zero

      if Y.Length = 0 then
         raise Constraint_Error with "division by zero";
      end if;

      --  Handle simple cases with special tests

      --  If X < Y then quotient is zero and remainder is X

      if Compare (X.Data, Y.Data, False, False) = Less_Than then
         Remainder := Normalize (X.Data);
         Quotient  := Normalize (Zero_Data);
         return;

      --  If both X and Y are less than 2**63-1, we can use Long_Long_Integer
      --  arithmetic. Note it is good not to do an accurate range check against
      --  Long_Long_Integer since -2**63 / -1 overflows.

      elsif (X.Length <= 1 or else (X.Length = 2 and then X.Data(1) < 2**31))
              and then
            (Y.Length <= 1 or else (Y.Length = 2 and then Y.Data(1) < 2**31))
      then
         declare
            A : constant LLI := abs (From_Big_Number (X));
            B : constant LLI := abs (From_Big_Number (Y));
         begin
            Quotient  := To_Big_Number (A / B);
            Remainder := To_Big_Number (A rem B);
            return;
         end;

      --  Easy case if divisor is one digit

      elsif Y.Length = 1 then
         declare
            ND  : Double_Digit;
            Div : constant Double_Digit := Double_Digit(Y.Data(1));

            Result : Digit_Vector (1 .. X.Length);
            Remdr  : Digit_Vector (1 .. 1);

         begin
            ND := 0;
            for J in 1 .. X.Length loop
               ND := Base * ND + Double_Digit(X.Data(J));
               Result (J) := SD (ND / Div);
               ND := ND rem Div;
            end loop;

            Quotient  := Normalize (Result);
            Remdr (1) := SD (ND);
            Remainder := Normalize (Remdr);
            return;
         end;
      end if;

      --  The complex full multi-precision case. We will employ algorithm
      --  D defined in the section "The Classical Algorithms" (sec. 4.3.1)
      --  of Donald Knuth's "The Art of Computer Programming", Vol. 2, 2nd
      --  edition. The terminology is adjusted for this section to match that
      --  reference.

      --  We are dividing X.Len digits of X (called u here) by Y.Len digits
      --  of Y (called v here), developing the quotient and remainder. The
      --  numbers are represented using Base, which was chosen so that we have
      --  the operations of multiplying to single digits (SD) to form a double
      --  digit (DD), and dividing a double digit (DD) by a single digit (SD)
      --  to give a single digit quotient and a single digit remainder.

      --  Algorithm D from Knuth

      --  Comments here with square brackets are directly from Knuth

      Algorithm_D : declare

         --  The following lower case variables correspond exactly to the
         --  terminology used in algorithm D.

         m : constant Length_Range := X.Length - Y.Length;
         n : constant Length_Range := Y.Length;
         b : constant Double_Digit := Base;

         u : Digit_Vector (0 .. m + n);
         v : Digit_Vector (1 .. n);
         q : Digit_Vector (0 .. m);
         r : Digit_Vector (1 .. n);

         u0 : SD renames u (0);
         v1 : SD renames v (1);
         v2 : SD renames v (2);

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

               u0 := SD (Carry);

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
               Borrow : SD;
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

                  u (j) := u (j) + SD (Carry);
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

         if not Discard_Quotient then
            Quotient := Normalize (q);
         end if;

         if not Discard_Remainder then
            declare
               Remdr : Double_Digit;

            begin
               Remdr := 0;
               for K in 1 .. n loop
                  Remdr := Base * Remdr + Double_Digit(u (m + K));
                  r (K) := SD (Remdr / d);
                  Remdr := Remdr rem d;
               end loop;

               pragma Assert (Remdr = 0);
            end;

            Remainder := Normalize (r);
         end if;
      end Algorithm_D;
    End Div_Rem;

    function Add
      (X, Y  : Digit_Vector;
       X_Neg : Boolean;
       Y_Neg : Boolean) return Big_Number is
    begin
	--  If signs are the same, we are doing an addition, it is convenient to
	--  ensure that the first operand is the longer of the two.

	if X_Neg = Y_Neg then
	    if X'Last < Y'Last then
		return Add (X => Y, Y => X, X_Neg => Y_Neg, Y_Neg => X_Neg);

		--  Here signs are the same, and the first operand is the longer

	    else
		pragma Assert (X_Neg = Y_Neg and then X'Last >= Y'Last);

		--  Do addition, putting result in Sum (allowing for carry)

		declare
		    Sum : Digit_Vector (0 .. X'Last);
		    RD  : Double_Digit;

		begin
		    RD := 0;
		    for J in reverse 1 .. X'Last loop
			RD := RD + Double_Digit (X (J));

			if J >= 1 + (X'Last - Y'Last) then
			    RD := RD + Double_Digit (Y (J - (X'Last - Y'Last)));
			end if;

			Sum (J) := LSD (RD);
			RD := RD / Base;
		    end loop;

		    Sum (0) := Digit (RD);
		    return Normalize (Sum, X_Neg);
		end;
	    end if;

	    --  Signs are different so really this is a subtraction, we want to make
	    --  sure that the largest magnitude operand is the first one, and then
	    --  the result will have the sign of the first operand.

	else
	    declare
		CR : constant Compare_Result := Compare (X, Y, False, False);

	    begin
		if CR = Equal then
		    return Normalize (Zero_Data);

		elsif CR = Less_Than then
		    return Add (X => Y, Y => X, X_Neg => Y_Neg, Y_Neg => X_Neg);

		else
		    pragma Assert (X_Neg /= Y_Neg and then CR = Greater_Than);

		    --  Do subtraction, putting result in Diff

		    declare
			Diff : Digit_Vector (1 .. X'Length);
			RD   : Double_Digit;

		    begin
			RD := 0;
			for J in reverse 1 .. X'Last loop
			    RD := RD + Double_Digit (X (J));

			    if J >= 1 + (X'Last - Y'Last) then
				RD := RD - Double_Digit (Y (J - (X'Last - Y'Last)));
			    end if;

			    Diff (J) := LSD (RD);
			    RD := (if RD < Base then 0 else -1);
			end loop;

			return Normalize (Diff, X_Neg);
		    end;
		end if;
	    end;
	end if;
    end Add;

    function Big_Abs (X : Big_Number) return Big_Number is
      ( Normalize (X.Data, Neg => False) );

    function Big_Add  (X, Y : Big_Number) return Big_Number is
      ( Add (X.Data, Y.Data, X.Negetive, Y.Negetive) );


    function Big_Sub (X, Y : Big_Number) return Big_Number is
      (if Y.Length = 0 then X
       else Add(X.Data,Y.Data,X.Negetive, not Y.Negetive)
      );

    function Big_Mul (X, Y : Big_Number) return Big_Number is

	--  Accumulate result (max length of result is sum of operand lengths)
	Result : Digit_Vector (1 .. X.Length + Y.Length) := (others => 0);

	--  Current result digit
	L : Length_Range;

	--  Result digit
	D : Double_Digit;

    begin
	for J in 1 .. X.Length loop
	    for K in 1 .. Y.Length loop
		L := Result'Last - (X.Length - J) - (Y.Length - K);
		D := Double_Digit (X.Data(J)) * Double_Digit (Y.Data(K)) + Double_Digit (Result (L));
		Result (L) := LSD (D);
		D := D / Base;

		--  D is carry which must be propagated

		while D /= 0 and then L >= 1 loop
		    L := L - 1;
		    D := D + Double_Digit (Result (L));
		    Result (L) := LSD (D);
		    D := D / Base;
		end loop;

		--  Must not have a carry trying to extend max length
		pragma Assert (D = 0);
	    end loop;
	end loop;

	--  Return result
	return Normalize (Result, X.Negetive xor Y.Negetive);
    end Big_Mul;

   function Big_Div  (X, Y : Big_Number) return Big_Number is
      Q, R : Big_Number := Zero;
   begin
      Div_Rem (X, Y, Q, R, Discard_Remainder => True);
      Q.Negetive := Q.Length > 0 and then (X.Negetive xor Y.Negetive);
      return Q;
   end Big_Div;

    function Big_Exp  (X, Y : Big_Number) return Big_Number is

	function "**" (X : Big_Number; Y : Digit) return Big_Number;
	--  Internal routine where we know right operand is one word

	----------
	-- "**" --
	----------

	function "**" (X : Big_Number; Y : Digit) return Big_Number is
	(case Y is
	    when 0 => --  X ** 0 is 1
		Normalize (One_Data),
	    when 1 => --  X ** 1 is X
		Normalize (X.Data),
	    when 2 => --  X ** 2 is X * X
		Big_Mul (X, X),

		--  For Y greater than 2, use the recursion

		--  Y even, X ** Y = (X ** (Y/2)) ** 2;
		--  Y odd,  X ** Y = (X ** (Y/2)) ** 2 * X;
	    when others =>
		(if Y mod 2 = 1 then X else One) *
                (X ** (Y/2)) ** 2
        );

	--  Start of processing for Big_Exp
    begin
	--  Error if right operand negative

	if Y.Negetive then
	    raise Constraint_Error with "exponentiation to negative power";

	--  X ** 0 is always 1 (including 0 ** 0, so do this test first)
	elsif Y.Length = 0 then
	    return Normalize (One_Data);

	--  0 ** X is always 0 (for X non-zero)
	elsif X.Length = 0 then
	    return Normalize (Zero_Data);

	--  (+1) ** Y = 1
	--  (-1) ** Y = +/-1 depending on whether Y is even or odd
	elsif X.Length = 1 and then X.Data (1) = 1 then
	    return Normalize
	      (X.Data, Neg => X.Negetive and then ((Y.Data(Y.Length) mod 2) = 1));

	--  If the absolute value of the base is greater than 1, then the
	--  exponent must not be bigger than one word, otherwise the result
	--  is ludicrously large, and we just signal Storage_Error right away.
	elsif Y.Length > 1 then
	    raise Storage_Error with "exponentiation result is too large";

	--  Special case (+/-)2 ** K, where K is 1 .. 31 using a shift
	elsif X.Length = 1 and then X.Data (1) = 2 and then Y.Data (1) < 32 then
	    declare
		D : constant Digit :=
		  Digit(Interfaces.Shift_Left (Interfaces.Unsigned_32'(1), Natural (Y.Data (1))));
	    begin
		return Normalize ((1 => D), X.Negetive);
	    end;

	--  Remaining cases have right operand of one word
	else
	    return X ** Y.Data (1);
	end if;
    end Big_Exp;

    function Big_Rem (X, Y : Big_Number) return Big_Number is
	Q, R : Big_Number := Zero;
    begin
	Div_Rem (X, Y, Q, R, Discard_Quotient => True);
	R.Negetive := R.Length > 0 and then X.Negetive;
	return R;
    end Big_Rem;

    function Big_Mod (X, Y : Big_Number) return Big_Number is
	Q, R : Big_Number:= Zero;
    begin
	--  If signs are same, result is same as Rem
	if X.Negetive = Y.Negetive then
	    return Big_Rem (X, Y);
	    --  Case where Mod is different
	else
	    --  Do division
	    Div_Rem (X, Y, Q, R, Discard_Quotient => True);

	    --  Zero result is unchanged
	    if R.Length = 0 then
		return R;

	    --  Otherwise adjust result
	    else
		declare
		    T1 : Big_Number := Big_Sub (Y, R);
		begin
		    T1.Negetive := Y.Negetive;
		    return T1;
		end;
	    end if;
	end if;
    end Big_Mod;


    function Big_Neg (X : Big_Number) return Big_Number is
    begin
	Return
	  (Length => X.Length, Data => X.Data, Negetive => not X.Negetive);
    end Big_Neg;


    function Big_EQ (X, Y : Big_Number) return Boolean is
      ( Compare (X.Data, Y.Data, X.Negetive, Y.Negetive) = Equal );

    function Big_GE (X, Y : Big_Number) return Boolean is
      ( Compare (X.Data, Y.Data, X.Negetive, Y.Negetive) /= Less_Than );

    function Big_GT (X, Y : Big_Number) return Boolean is
      ( Compare (X.Data, Y.Data, X.Negetive, Y.Negetive) = Greater_Than );

    function Big_LE (X, Y : Big_Number) return Boolean is
      ( Compare (X.Data, Y.Data, X.Negetive, Y.Negetive) /= Greater_Than );

    function Big_LT (X, Y : Big_Number) return Boolean is
      ( Compare (X.Data, Y.Data, X.Negetive, Y.Negetive) = Less_Than );



    function "+"   (Left, Right : Big_Number) return Big_Number renames Big_Add;
    function "-"   (Left, Right : Big_Number) return Big_Number renames Big_Sub;
    function "*"   (Left, Right : Big_Number) return Big_Number renames Big_Mul;
    function "/"   (Left, Right : Big_Number) return Big_Number renames Big_Div;
    function "**"  (Left, Right : Big_Number) return Big_Number renames Big_Exp;
    function "MOD" (Left, Right : Big_Number) return Big_Number renames Big_Mod;
    function "REM" (Left, Right : Big_Number) return Big_Number renames Big_Rem;
    function "-"   (Left        : Big_Number) return Big_Number renames Big_Neg;
    function "ABS" (Left        : Big_Number) return Big_Number renames Big_Abs;

    function "="   (Left, Right : Big_Number) return Boolean renames Big_EQ;
    function ">="  (Left, Right : Big_Number) return Boolean renames Big_GE;
    function "<="  (Left, Right : Big_Number) return Boolean renames Big_LE;
    function ">"   (Left, Right : Big_Number) return Boolean renames Big_GT;
    function "<"   (Left, Right : Big_Number) return Boolean renames Big_LT;

    function Convert (Item : Long_Long_Integer) return Big_Number renames To_Big_Number;
    function Convert (Item : Big_Number) return Long_Long_Integer is
      (From_Big_Number(Item)) with SPARK_Mode => Off;

End ASN_1.Internals.Numerics;
