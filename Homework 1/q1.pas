program PascalTriangle;
{
    Here you may write helper functions and procedures.
    Please do.
    
}
type IntArray = array [1..15] of integer;
function construct(i: integer):IntArray; 
    var line:IntArray;
    var prev_line:IntArray;
    var j:integer;
    begin
        for j := 1 to 15 do line[j] := 0;
        line[1] := 1;
        if i = 1 then construct := line
        else
            begin
            prev_line := construct(i-1);
            line[1] := 1;
            line[i] := 1;
            for j:= 2 to i-1 do
                begin 
                    line[j] := prev_line[j-1] + prev_line[j]
                end;
            construct := line;
            end;
    end;

var n: integer;
var line: IntArray;
var k, i, l: integer;
begin
for k:= 1 to 15 do line[k] := 0;
ReadLn(n);
for i := 1 to n do
    begin
        {
            Construct the new line and then print it.
        }
        line := construct(i);
        for l:=1 to i do
        begin
            if l < i then Write(line[l], ' ') else Write(line[l]);
            
        end;
        WriteLn();
        
    end;
end.
