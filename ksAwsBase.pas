{*******************************************************************************
*                                                                              *
*  ksAwsConst - Amazon Web Service Base Classes                                *
*                                                                              *
*  https://github.com/gmurt/ksAws                                              *
*                                                                              *
*  Copyright 2020 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksAwsBase;

interface

uses Classes, ksAwsHttpIntf, Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc;

type
  TksAwsRegion = (awsEuCentral1, awsEuNorth1, awsEuSouth1, awsEuWest1, awsEuWest2, awsEuWest3, awsUsEast1, awsUsEast2, awsUsWest1, awsUsWest2);

  TksAwsBaseService = class(TInterfacedObject)
  private
    FAccessKey: string;
    FSecretKey: string;
    FRegion: TksAwsRegion;
    function GetRegionStr: string;
    function RegionToStr(ARegion: TksAwsRegion): string;
    procedure GetHeaders(ARequestTime: TDateTime; AHost: string; APayload: TStream; AHeaders: TStrings);
    function GetUrl(AHost, APath: string; AParams: TStrings): string;
  protected
    function GetApiVersion: string; virtual;
    function GetHost: string; virtual;
    function GetHostSubdomain: string; virtual;
    function GetPayload(AStr: string): string;
    function GenerateUrl(ASubDomain, APath: string): string;
    function GetServiceName: string; virtual; abstract;
    function GenerateCanonicalRequest(AVerb, AHost, URI: string; APayload: TStream; AHeaders, AQueryValues: TStrings): string; overload;

    //function ExecuteHttpNew(AVerb, AAction, AHost, APath: string; AExtraHeaders, AQueryParams: TStrings; AContent: string; const AUseRegion: Boolean = True; const AResponseStream: TStream = nil): IksAwsHttpResponse; overload;


    //function ExecuteHttp(AVerb, AAction, AHost, APath: string; AContent: string): IksAwsHttpResponse; overload;
    function ExecuteHttp(AVerb, AAction, AHost, APath: string; AExtraHeaders, AQueryParams: TStrings; AContent: string; const AUseRegion: Boolean = True; const AResponseStream: TStream = nil): IksAwsHttpResponse; overload;
    function ExecuteHttp(AVerb, AAction, AHost, APath: string; AExtraHeaders, AQueryParams: TStrings; AContent: TStream; const AUseRegion: Boolean = True; const AResponseStream: TStream = nil): IksAwsHttpResponse; overload;

    //function ExecuteHttp(AVerb, AAction, AHost, APath, APayload: string; AExtraHeaders, AQueryParams: TStrings; const AUseRegion: Boolean = True; const AResponseStream: TStream = nil): IksAwsHttpResponse; overload;
    function ExecuteHttpXml(AVerb, AAction, AHost, APath, APayload: string; AExtraHeaders, AQueryParams: TStrings; const AUseRegion: Boolean = True; const AResponseStream: TStream = nil): IXmlDocument;
  public
    constructor Create(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion);
    property AccessKey: string read FAccessKey;
    property SecretKey: string read FSecretKey;
    property Region: TksAwsRegion read FRegion;
    property RegionStr: string read GetRegionStr;
    property ServiceName: string read GetServiceName;
    property Host: string read GetHost;
  end;

  function StringToRegion(ARegionStr: string): TksAwsRegion;

implementation

{$I include.inc}

uses ksAwsConst, ksAwsHash, SysUtils, DateUtils, IdGlobal;

function StringToRegion(ARegionStr: string): TksAwsRegion;
begin
  ARegionStr := Trim(ARegionStr.ToLower);
  if ARegionStr = C_RGN_EU_CENTRAL_1 then Result := awsEuCentral1;
  if ARegionStr = C_RGN_EU_NORTH_1 then Result := awsEuNorth1;
  if ARegionStr = C_RGN_EU_SOUTH_1 then Result := awsEuSouth1;
  if ARegionStr = C_RGN_EU_WEST_1 then Result := awsEuWest1;

  if ARegionStr = C_RGN_EU_WEST_2 then Result := awsEuWest2;
  if ARegionStr = C_RGN_EU_WEST_3 then Result := awsEuWest3;
  if ARegionStr = C_RGN_US_EAST_1 then Result := awsUsEast1;
  if ARegionStr = C_RGN_US_EAST_2 then Result := awsUsEast2;
  if ARegionStr = C_RGN_US_WEST_1 then Result := awsUsWest1;
  if ARegionStr = C_RGN_US_WEST_2 then Result := awsUsWest2;
end;



{ TksAwsBaseService }

constructor TksAwsBaseService.Create(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion);
begin
  inherited Create;
  FAccessKey := AAccessKey;
  FSecretKey := ASecretKey;
  FRegion := ARegion;
end;


function TksAwsBaseService.GetUrl(AHost, APath: string; AParams: TStrings): string;
var
  ICount: integer;
begin
  Result := '';
  if AParams <> nil then
  begin
    for ICount := 0 to AParams.Count-1 do
    begin
      if ICount = 0 then
        Result := Result + '?'
      else
        Result := Result + '&';
      Result := Result + AParams.Names[ICount]+'='+ParamEncode(AParams.ValueFromIndex[ICount]);    end;
  end;
  Result := C_PROTOCOL+'://'+AHost+APath+Result;
end;

function TksAwsBaseService.RegionToStr(ARegion: TksAwsRegion): string;
begin
  case ARegion of
    awsEuCentral1: Result := C_RGN_EU_CENTRAL_1;
    awsEuNorth1  : Result := C_RGN_EU_NORTH_1;
    awsEuSouth1  : Result := C_RGN_EU_SOUTH_1;
    awsEuWest1   : Result := C_RGN_EU_WEST_1;
    awsEuWest2   : Result := C_RGN_EU_WEST_2;
    awsEuWest3   : Result := C_RGN_EU_WEST_3;
    awsUsEast1   : Result := C_RGN_US_EAST_1;
    awsUsEast2   : Result := C_RGN_US_EAST_2;
    awsUsWest1   : Result := C_RGN_US_WEST_1;
    awsUsWest2   : Result := C_RGN_US_WEST_2;
  end;
end;

function TksAwsBaseService.GenerateUrl(ASubDomain, APath: string): string;
begin
  Result := C_PROTOCOL+'://';
  if ASubDomain <> '' then Result := Result + UrlEncode(ASubDomain);// TNetEncoding.URL.Encode(ASubDomain)+'.';
  Result := Result + ServiceName+'.'+RegionStr+'.'+C_AMAZON_DOMAIN+'/';
  if APath <> '' then
    Result := Result + UrlEncode(APath);
end;

function TksAwsBaseService.GenerateCanonicalRequest(AVerb, AHost, URI: string; APayload: TStream; AHeaders, AQueryValues: TStrings): string;
var
  ICount:
  integer;
  AHash: string;
begin
  AHash := GetHashSHA256Hex(APayload);
  Result := UrlEncode(AVerb) +C_LF;
  Result := Result + UrlEncode(URI) +C_LF;
  if AQueryValues <> nil then
  begin
    (AQueryValues as TStringList).Sort;
    for ICount := 0 to AQueryValues.Count-1 do
    begin
      Result := Result + AQueryValues.Names[ICount]+'='+ParamEncode(AQueryValues.ValueFromIndex[ICount]);
      if ICount < AQueryValues.Count-1 then Result := Result + '&';
    end;
  end;
  Result := Result + C_LF;;
  for ICount := 0 to AHeaders.Count-1 do
    Result := Result + AHeaders.Names[ICount]+':'+AHeaders.ValueFromIndex[ICount]+C_LF;
  Result := Result + C_LF;
  for ICount := 0 to AHeaders.Count-1 do
  begin
    Result := Result + AHeaders.Names[ICount];
    if ICount < AHeaders.Count-1 then
      Result := Result + ';';
  end;
  Result := Result + C_LF;
  Result := Result + AHash;
end;

function TksAwsBaseService.GetApiVersion: string;
begin
  //
end;

procedure TksAwsBaseService.GetHeaders(ARequestTime: TDateTime; AHost: string; APayload: TStream; AHeaders: TStrings);
begin
  AHeaders.Values['host'] := Trim(UrlEncode(AHost));
  AHeaders.Values['x-amz-content-sha256'] := GetHashSHA256Hex(APayload);
  AHeaders.Values['x-amz-date'] := FormatDateTime(C_AMZ_DATE_FORMAT, ARequestTime, TFormatSettings.Create('en-US'));
  (AHeaders as TStringList).Sort;

end;

function TksAwsBaseService.GetHost: string;
begin
  Result := Format('%s.%s.%s', [GetHostSubdomain, RegionStr, C_AMAZON_DOMAIN]);
end;

function TksAwsBaseService.GetHostSubdomain: string;
begin
  Result := GetServiceName;
end;

function TksAwsBaseService.GetPayload(AStr: string): string;
begin
  Result := AStr;
  Result := StringReplace(AStr, '%REGION%', GetRegionStr, [rfReplaceAll]);
end;

function TksAwsBaseService.GetRegionStr: string;
begin
  Result := RegionToStr(FRegion);
end;

function TksAwsBaseService.ExecuteHttpXml(AVerb, AAction, AHost, APath, APayload: string; AExtraHeaders, AQueryParams: TStrings; const AUseRegion: Boolean = True; const AResponseStream: TStream = nil): IXmlDocument;
var
  AResponse: IksAwsHttpResponse;
begin
  Result := TXMLDocument.Create(nil);
  AResponse := ExecuteHttp(AVerb, AAction, AHost, APath, AExtraHeaders, AQueryParams, APayload, AUseRegion, AResponseStream);
  Result.LoadFromXML(AResponse.ContentAsString);
end;
                 {
function TksAwsBaseService.ExecuteHttpNew(AVerb, AAction, AHost, APath: string; AExtraHeaders, AQueryParams: TStrings; AContent: string; const AUseRegion: Boolean = True; const AResponseStream: TStream = nil): IksAwsHttpResponse;
var
  ARegion: string;
  AHash: string;
  AHeaders: TStrings;
  AParams: TStrings;
begin
  ARegion := RegionStr;
  if AUseRegion = False then
    ARegion := RegionToStr(awsUsEast1);

  AHeaders := TStringList.Create;
  AParams := TStringList.Create;
  try
     AHash := GetHashSHA256Hex(AContent);


  finally
    AHeaders.Free;
    AParams.Free;
  end;
end;        }

function TksAwsBaseService.ExecuteHttp(AVerb,
                                       AAction,
                                       AHost,
                                       APath: string;
                                       AExtraHeaders,
                                       AQueryParams: TStrings;
                                       AContent: string;
                                       const AUseRegion: Boolean = True;
                                       const AResponseStream: TStream = nil): IksAwsHttpResponse;
var
  AStream: TStringStream;
begin
  AStream := TStringStream.Create(AContent, TEncoding.UTF8);
  try
    Result := ExecuteHttp(AVerb,
                          AAction,
                          AHost,
                          APath,
                          AExtraHeaders,
                          AQueryParams,
                          AStream,
                          AUseRegion,
                          AResponseStream);
  finally
    AStream.Free;
  end;

end;

function TksAwsBaseService.ExecuteHttp(AVerb, AAction, AHost, APath: string; AExtraHeaders, AQueryParams: TStrings; AContent: TStream; const AUseRegion: Boolean = True; const AResponseStream: TStream = nil): IksAwsHttpResponse;
var
  AHttp: IksAwsHttp;
  AHeaders: TStrings;
  ACanonical: string;
  AStringToSign: string;
  ASignature: string;
  AAmzDate: string;
  AAuthHeader: string;
  ANowUtc: TDateTime;
  AShortDate: string;
  AHash: string;
  AUrl: string;
  ADelimitedHeaders: string;
  ARegion: string;
  ICount: integer;
  AParams: TStrings;
begin
  ARegion := RegionStr;
  if AUseRegion = False then
    ARegion := RegionToStr(awsUsEast1);

  AHeaders := TStringList.Create;
  AParams := TStringList.Create;
  try
    if AQueryParams <> nil then
      AParams.AddStrings(AQueryParams);
    AParams.Values['Action'] := AAction;
    AParams.Values['Version'] := GetApiVersion;




    if Pos('/', APath) <> 1 then
      APath := '/'+APath;
    AHash := GetHashSHA256Hex(AContent);
    AContent.Position := 0;

    if AExtraHeaders <> nil then
      AHeaders.AddStrings(AExtraHeaders);

    ANowUtc := TTimeZone.Local.ToUniversalTime(Now);
    GetHeaders(ANowUtc, AHost, AContent, AHeaders);


    ADelimitedHeaders := '';
    for ICount := 0 to AHeaders.Count-1 do
    begin
      ADelimitedHeaders := LowerCase(ADelimitedHeaders+AHeaders.Names[ICount]);
      if ICount < AHeaders.Count-1 then
        ADelimitedHeaders := ADelimitedHeaders + ';';
    end;
    AAmzDate := FormatDateTime(C_AMZ_DATE_FORMAT, ANowUtc, TFormatSettings.Create('en-US'));
    AShortDate := FormatDateTime(C_SHORT_DATE_FORMAT, ANowUtc, TFormatSettings.Create('en-US'));

    ACanonical := GenerateCanonicalRequest(AVerb, AHost, APath, AContent, AHeaders, AParams);
    AStringToSign := C_HASH_ALGORITHM +C_LF +
                     AAmzDate +C_LF+
                     FormatDateTime(C_SHORT_DATE_FORMAT, ANowUtc) +'/'+ ARegion +'/'+ GetServiceName +'/aws4_request' +C_LF+
                     GetHashSHA256Hex(ACanonical);
    ASignature := GenerateSignature(ANowUtc, AStringToSign, FSecretKey, ARegion, GetServiceName);
    AAuthHeader := C_HASH_ALGORITHM+' Credential='+FAccessKey+'/'+
                   FormatDateTime(C_SHORT_DATE_FORMAT, ANowUtc)+'/'+
                   ARegion+'/'+
                   GetServiceName+'/'+
                   'aws4_request,SignedHeaders='+ADelimitedHeaders+',Signature='+ASignature;

    AHeaders.Values['Authorization'] := AAuthHeader;
    AUrl := GetUrl(AHost, APath, AParams);

    AHttp := CreateAwsHttp;

    if AVerb = C_HEAD then Result := AHttp.Head(AUrl, AHeaders);
    if AVerb = C_GET then Result := AHttp.Get(AUrl, AHeaders, AResponseStream);
    if AVerb = C_PUT then Result := AHttp.Put(AUrl, AContent, AHeaders, AResponseStream);
    if AVerb = C_POST then Result := AHttp.Post(AUrl, AContent, AHeaders, AResponseStream);
    if AVerb = C_DELETE then Result := AHttp.Delete(AUrl, AHeaders, AResponseStream);
  finally
    AHeaders.Free;
    AParams.Free;
  end;
end;

      {
function TksAwsBaseService.ExecuteHttp(AVerb, AAction, AHost, APath, APayload: string; AExtraHeaders, AQueryParams: TStrings; const AUseRegion: Boolean = True; const AResponseStream: TStream = nil): IksAwsHttpResponse;
var
  AContent: TStringStream;
begin
  AContent := TStringStream.Create(APayload);
  try
    Result := ExecuteHttp(AVerb, AAction, AHost, APath, AExtraHeaders, AQueryParams, AContent, AUseRegion, AResponseStream);
  finally
    AContent.Free;
  end;
end;   }{

function TksAwsBaseService.ExecuteHttp(AVerb, AAction, AHost, APath, AContent: string): IksAwsHttpResponse;
var
  AStrings: TStrings;
  AStream: TMemoryStream;
begin
  AStrings := TStringList.Create;
  AStream := TMemoryStream.Create;
  try
    AStrings.Text := Trim(AContent);
    AStrings.SaveToStream(AStream);
    AStream.Position := 0;
    Result := ExecuteHttp(AVerb, AAction, AHost, APath, nil, nil, AStream);
  finally
    AStream.Free;
    AStrings.Free;
  end;
end;
        }
end.
