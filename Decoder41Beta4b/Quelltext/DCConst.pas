unit DCConst;

interface

uses
  Classes, SysUtils;

type
  TLanguageEntry = record
    name: string;
    text: string;
  end;

function max_2(A, B: integer): integer;
function max_3(A, B, C: integer): integer;
function max_4(A, B, C, D: integer): integer;
function max_7(A, B, C, D, E, F, G: integer): integer;

function paramstr_firstposition(str: string): integer;
function paramzeile_firstposition(str: string): integer;
function ZaehleLinien(str: string): integer;
function GebeLinieaus(str: string; linie: integer): string;

const
  // Global
  DC4Ver: String = '4.1 Public Beta 4b';

  // ConfigForm
  activator = 'activator.exe';
  StdGarbarge = 5;

var
  ParamZeile: string;

implementation

function max_2(A, B: integer): integer;
begin
  result := 0;
  if (A >= B) then result := A;
  if (B >= A) then result := B;
end;

function max_3(A, B, C: integer): integer;
begin
  result := 0;
  if (A >= B) and (A >= C) then result := A;
  if (B >= A) and (B >= C) then result := B;
  if (C >= A) and (C >= B) then result := C;
end;

function max_4(A, B, C, D: integer): integer;
begin
  result := 0;
  if (A >= B) and (A >= C) and (A >= D) then result := A;
  if (B >= A) and (B >= C) and (B >= D) then result := B;
  if (C >= A) and (C >= B) and (C >= D) then result := C;
  if (D >= A) and (D >= B) and (D >= C) then result := D;
end;

function max_7(A, B, C, D, E, F, G: integer): integer;
begin
  result := 0;
  if (A >= B) and (A >= C) and (A >= D) and (A >= E) and (A >= F) and (A >= G) then result := A;
  if (B >= A) and (B >= C) and (B >= D) and (B >= E) and (B >= F) and (B >= G) then result := B;
  if (C >= A) and (C >= B) and (C >= D) and (C >= E) and (C >= F) and (C >= G) then result := C;
  if (D >= A) and (D >= B) and (D >= C) and (D >= E) and (D >= F) and (D >= G) then result := D;
  if (E >= A) and (E >= B) and (E >= C) and (E >= D) and (E >= F) and (E >= G) then result := E;
  if (F >= A) and (F >= B) and (F >= C) and (F >= D) and (F >= E) and (F >= G) then result := F;
  if (G >= A) and (G >= B) and (G >= C) and (G >= D) and (G >= E) and (G >= F) then result := G;
end;

function paramstr_firstposition(str: string): integer;
var
  i: integer;
begin
  result := -1;
  for i  := 1 to paramcount() do
  begin
    if paramstr(i) = lowercase(str) then
      result := i;
  end;
end;

function paramzeile_firstposition(str: string): integer;
var
  i: integer;
begin
  result := -1;
  for i  := 1 to ZaehleLinien(paramzeile) do
  begin
    if GebeLinieaus(paramzeile, i) = lowercase(str) then
      result := i;
  end;
end;

function ZaehleLinien(str: string): integer;
var
  temp: tstringlist;
begin
  temp := tstringlist.Create;
  try
    temp.text := str;
    result := temp.count;
  finally
    temp.free;
  end;
end;

function GebeLinieaus(str: string; linie: integer): string;
var
  temp: tstringlist;
begin
  temp := tstringlist.Create;
  try
    temp.text := str;
    if linie <= temp.Count then
      result := temp.Strings[linie-1]
    else
      result := '';
  finally
    temp.free;
  end;
end;

end.
