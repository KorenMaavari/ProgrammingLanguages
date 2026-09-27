program q2;
type letter = 'a'..'z';
type hist = array[letter] of integer;


procedure init_hist (name: string; var histo: hist);
var i:integer;
var c:letter;
begin
    for c:= 'a' to 'z' do
    begin
        histo[c] := 0
    end;

    for i:=1 to Length(name) do
    begin
        histo[name[i]] := histo[name[i]] + 1;
    end;
end;

var name1, name2, name3 : string;
var hist1, hist2, hist3 : hist;
var valid, valid1, valid2:boolean;
var sum:integer;
var i:integer;
var c:letter;



begin
    ReadLn(name1);
    ReadLn(name2);
    ReadLn(name3);
    valid := True;
    valid1 := False;
    valid2 := False;

    init_hist(name1, hist1);
    init_hist(name2, hist2);
    init_hist(name3, hist3);
    for i:=1 to Length(name3) do
    begin
        if ((hist1[name3[i]] = 0) and (hist2[name3[i]] = 0)) then valid := False
    end;
    for c:= 'a' to 'z' do
    begin
        if (hist1[c] > 0) and (hist2[c] > 0) then valid := False;
        if (hist1[c] > 0) and (hist3[c] > 0) then valid1 := True;
        if (hist2[c] > 0) and (hist3[c] > 0) then valid2 := True;

    end;
    if valid and valid1 and valid2 then WriteLn('TRUE') else WriteLn('FALSE');
    for c:= 'a' to 'z' do
    begin
        sum := hist1[c] + hist2[c] + hist3[c];
        if sum > 0 then WriteLn(c, ' ', sum);
    end;
end.
