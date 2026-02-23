unit ksAwsXml;

interface

function GetXmlTagValue(const AXml, ATag: string): string;
function GetXmlBlock(const AXml, ATag: string; AOffset: Integer; out ABlockEnd: Integer): string;

implementation

uses StrUtils;

function GetXmlTagValue(const AXml, ATag: string): string;
var
  OpenStart, OpenEnd, CloseStart: Integer;
  TagLen: Integer;
  CloseTag: string;
begin
  Result := '';
  TagLen := Length(ATag);
  CloseTag := '</' + ATag + '>';

  // Find "<Tag"
  OpenStart := Pos('<' + ATag, AXml);
  if OpenStart = 0 then Exit;

  // Ensure next char is valid delimiter (space, >, /)
  if not (AXml[OpenStart + TagLen + 1] in [' ', '>', '/']) then
    Exit;

  // Find end of opening tag
  OpenEnd := PosEx('>', AXml, OpenStart);
  if OpenEnd = 0 then Exit;

  // Self-closing tag
  if AXml[OpenEnd - 1] = '/' then
    Exit;

  // Find closing tag
  CloseStart := PosEx(CloseTag, AXml, OpenEnd + 1);
  if CloseStart = 0 then Exit;

  Result := Copy(AXml, OpenEnd + 1, CloseStart - OpenEnd - 1);
end;

function GetXmlBlock(const AXml, ATag: string; AOffset: Integer; out ABlockEnd: Integer): string;
var
  OpenStart, OpenEnd, CloseStart: Integer;
  TagLen: Integer;
  CloseTag: string;
begin
  Result := '';
  ABlockEnd := 0;

  TagLen := Length(ATag);
  CloseTag := '</' + ATag + '>';

  OpenStart := PosEx('<' + ATag, AXml, AOffset);
  if OpenStart = 0 then Exit;

  if not (AXml[OpenStart + TagLen + 1] in [' ', '>', '/']) then
    Exit;

  OpenEnd := PosEx('>', AXml, OpenStart);
  if OpenEnd = 0 then Exit;

  if AXml[OpenEnd - 1] = '/' then
  begin
    ABlockEnd := OpenEnd;
    Exit;
  end;

  CloseStart := PosEx(CloseTag, AXml, OpenEnd + 1);
  if CloseStart = 0 then Exit;

  ABlockEnd := CloseStart + Length(CloseTag);
  Result := Copy(AXml, OpenEnd + 1, CloseStart - OpenEnd - 1);
end;

end.
