
/////////////////////////////////////////
//  Dodgson-style triangularization
/////////////////////////////////////////
Dodgson := function(M, v, quotring, basering, debug)
    // M: matrix over some ring
    // v: polynomial variable (in some polynomial ring over that ring)
    A := M;

    A := ChangeRing(A, quotring);

    B := M;
    B := ChangeRing(B, quotring);

    n := Nrows(A);
    m := Ncols(A);
    R := BaseRing(A);
    R2 := BaseRing(Parent(v));
   
    for k in [1..n] do
        for i in [k+1..n] do
            
            if k gt 1 then
                if A[k-1][k-1] eq 0 then
                    continue;
                end if;
            end if;

            for j in [k..m] do
                if k eq 1 then
                    target := R!1;
                else
                    target := A[k-1][k-1];
                end if;

                // B[i][j] := ((A[k][k] * A[i][j] - A[i][k] * A[k][j])/target);
                B[i][j] := ((A[k][k] * A[i][j] - A[i][k] * A[k][j]));
               
            
            end for;
        end for;

        A := B;
    end for;


    if debug then
        print Nrows(A), "x", Ncols(A), " triangularized matrix:";
        PrettyPrintMatrix(A);
    end if;

    return A;
end function;