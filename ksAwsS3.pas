{*******************************************************************************
*                                                                              *
*  ksAwsSes - Amazon S3 Interface for Delphi                                   *
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

unit ksAwsS3;

interface

uses Classes, ksAwsBase;

type
  TksS3Acl = (ksS3Private, ksS3PublicRead, ksS3PublicReadWrite, ksS3AuthenticatedRead);

  TksS3PutOptions = record
    Acl: TksS3Acl;
  end;

  IksAwsS3Object = interface
    ['{C9390D73-62A6-43F1-AAE1-479693BAAAFC}']
    function GetKey: string;
    function GetStream: TStream;
    function GetSize: integer;
    function GetETag: string;
    function GetLastModified: string;
    function GetAsText: string;
    procedure SaveToFile(AFilename: string);
    procedure SetAsText(const Value: string);
    property Key: string read GetKey;
    property Stream: TStream read GetStream;
    property Size: integer read GetSize;
    property ETag: string read GetETag;
    property LastModified: string read GetLastModified;
    property AsText: string read GetAsText write SetAsText;
  end;

  IksAwsS3 = interface
    ['{BD814B29-8F03-425F-BF47-FECBEA49D133}']
    function GetBucketLocation(ABucketName: string): string;
    function BucketExists(ABucketName: string): Boolean;
    function GetObject(ABucketName, AObjectName: string): IksAwsS3Object;
    function DeleteObject(ABucketName, AObjectName: string): Boolean;
    function CreateBucket(ABucketName: string; AAcl: TksS3Acl): Boolean;
    function DeleteBucket(ABucketName: string): Boolean;
    procedure GetBuckets(ABuckets: TStrings);
    procedure GetBucket(ABucketName: string; AContents: TStrings);
    procedure PutObject(ABucketName, APath: string; AObject: TStream; AOptions: TksS3PutOptions; const AExtraHeaders: TStrings = nil); overload;
    procedure PutObject(ABucketName, APath, AFilename: string; AOptions: TksS3PutOptions; const AExtraHeaders: TStrings = nil); overload;
  end;

  function CreateAwsS3(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion): IksAwsS3;

implementation

uses ksAwsConst, SysUtils, System.DateUtils, HttpApp,
  Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc, ksAwsHash, ksAwsHttpIntf;

type
  TksAwsS3Object = class(TInterfacedObject, IksAwsS3Object)
  private
    FKey: string;
    FEtag: string;
    FStream: TStream;
    FLastModified: string;
    function GetETag: string;
    function GetStream: TStream;
    function GetSize: integer;
    function GetKey: string;
    function GetLastModified: string;
    function GetAsText: string;
    procedure SetAsText(const Value: string);
  protected
    procedure SaveToFile(AFilename: string);

    property Key: string read GetKey;
    property ETag: string read GetETag;
    property LastModified: string read GetLastModified;
    property Size: integer read GetSize;
    property Stream: TStream read GetStream;


  public
    constructor Create(AKey: string;
                       AETag: string;
                       ALastModified: string;
                       AStream: TStream); virtual;
    destructor Destroy; override;
  end;

  TksAwsS3 = class(TksAwsBaseService, IksAwsS3)
  private
    function GetAclString(AAcl: TksS3Acl): string;
  protected
    function GetBucketLocation(ABucketName: string): string;
    function GetServiceName: string; override;
    function BucketExists(ABucketName: string): Boolean;
    function GetObject(ABucketName, AObjectName: string): IksAwsS3Object;
    function CreateBucket(ABucketName: string; AAcl: TksS3Acl): Boolean;
    function DeleteBucket(ABucketName: string): Boolean;
    procedure GetBuckets(AStrings: TStrings);
    procedure GetBucket(ABucketName: string; AStrings: TStrings);
    function DeleteObject(ABucketName, AObjectName: string): Boolean;
    procedure PutObject(ABucketName, APath: string; AContent: TStream; AOptions: TksS3PutOptions; const AExtraHeaders: TStrings = nil); overload;
    procedure PutObject(ABucketName, APath, AFilename: string; AOptions: TksS3PutOptions; const AExtraHeaders: TStrings = nil); overload;
  end;

function CreateAwsS3(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion): IksAwsS3;
begin
  Result := TksAwsS3.Create(AAccessKey, ASecretKey, ARegion);
end;

{ TksAwsS3Object }

constructor TksAwsS3Object.Create(AKey: string;
                                  AETag: string;
                                  ALastModified: string;
                                  AStream: TStream);
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  AStream.Position := 0;
  FStream.CopyFrom(AStream, AStream.Size);
  FStream.Position := 0;
  FEtag := AETag;
  FKey := AKey;
  FLastModified := ALastModified;
end;

destructor TksAwsS3Object.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TksAwsS3Object.GetAsText: string;
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    FStream.Position := 0;
    AStrings.LoadFromStream(FStream);
    Result := AStrings.Text;
  finally
    AStrings.Free;
  end;
end;

function TksAwsS3Object.GetETag: string;
begin
  Result := FEtag;
end;

function TksAwsS3Object.GetKey: string;
begin
  Result := FKey;
end;

function TksAwsS3Object.GetLastModified: string;
begin
  Result := FLastModified;
end;

function TksAwsS3Object.GetSize: integer;
begin
  Result := FStream.Size;
end;

function TksAwsS3Object.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TksAwsS3Object.SaveToFile(AFilename: string);
begin
  (FStream as TMemoryStream).SaveToFile(AFilename);
end;

procedure TksAwsS3Object.SetAsText(const Value: string);
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    (FStream as TMemoryStream).Clear;
    AStrings.Text := Value;
    AStrings.SaveToStream(FStream);
    FStream.Position := 0;
  finally
    AStrings.Free;
  end;
end;

{ TksAwsS3 }

function TksAwsS3.BucketExists(ABucketName: string): Boolean;

begin
  Result := ExecuteHttp(C_HEAD, '', ABucketName+'.'+Host, '/', nil, nil, '').StatusCode = 200;
end;

function TksAwsS3.CreateBucket(ABucketName: string; AAcl: TksS3Acl): Boolean;
var
  AResponse: string;
  APayload: string;
  AHeaders: TStrings;
begin
  AHeaders := TStringList.Create;
  try
    AHeaders.Values['x-amz-acl'] := GetAclString(AAcl);
    APayload := GetPayload(C_PAYLOAD_CREATE_BUCKET);
    AResponse := ExecuteHttp(C_PUT, '', ABucketName+'.'+Host, '/', AHeaders, nil, APayload).ContentAsString;
    Result := AResponse = '';
  finally
    AHeaders.Free;
  end;
end;

function TksAwsS3.DeleteBucket(ABucketName: string): Boolean;
var
  AResponse: string;
begin
  AResponse := ExecuteHttp(C_DELETE, '', ABucketName+'.'+Host, '/', nil, nil, '').ContentAsString;
  Result := AResponse = '';
end;


function TksAwsS3.GetAclString(AAcl: TksS3Acl): string;
begin
  case AAcl of
    ksS3Private: Result := 'private';
    ksS3PublicRead: Result := 'public-read';
    ksS3PublicReadWrite: Result := 'public-read-write';
    ksS3AuthenticatedRead: Result := 'authenticated-read';
  end;
end;

procedure TksAwsS3.GetBucket(ABucketName: string;
                             AStrings: TStrings);
var
  AXml: IXMLDocument;
  AContents: IXMLNode;
  ICount: integer;
  AObject: IXMLNode;
begin
  AStrings.Clear;
  AXml := ExecuteHttpXml(C_GET, '', ABucketName+'.'+Host, '/', '', nil, nil);
  AContents := AXml.ChildNodes['ListBucketResult'];
  for ICount := 0 to AContents.ChildNodes.Count-1 do
  begin
    AObject := AContents.ChildNodes[ICount];
    if AObject.NodeName = 'Contents' then
      AStrings.Add(AObject.ChildValues['Key']);
  end;
end;

function TksAwsS3.GetBucketLocation(ABucketName: string): string;
var
  AResponse: IksAwsHttpResponse;
begin
  AResponse := ExecuteHttp(C_HEAD, '', ABucketName+'.'+Host, '', nil, nil, '', True, nil);
  Result := AResponse.HeaderValue['x-amz-bucket-region'];
end;

procedure TksAwsS3.GetBuckets(AStrings: TStrings);
var
  AXml: IXMLDocument;
  ABuckets: IXMLNode;
  ABucket: IXMLNode;
  ICount: integer;
begin
  AXml := ExecuteHttpXml(C_GET, '', Host, '', '', nil, nil);
  ABuckets := AXml.ChildNodes['ListAllMyBucketsResult'];
  ABuckets := ABuckets.ChildNodes['Buckets'];
  for ICount := 0 to ABuckets.ChildNodes.Count-1 do
  begin
    ABucket := ABuckets.ChildNodes[ICount];
    AStrings.Add(ABucket.ChildNodes['Name'].Text);
  end;
end;

function TksAwsS3.GetObject(ABucketName, AObjectName: string): IksAwsS3Object;
var
  AResponse: IksAwsHttpResponse;
  AFilename: string;
  AParams: TStrings;
  AStream: TStream;
begin
  AParams := TStringList.Create;
  AStream := TMemoryStream.Create;
  try
    AResponse := ExecuteHttp(C_GET, '', ABucketName+'.'+Host, AObjectName, nil, AParams, '', True, AStream);
  AFilename := AObjectName;
  while Pos('/', AFilename) > 0 do
    AFilename := Copy(AFilename, Pos('/', AFilename)+1, Length(AFilename));

  Result := TksAwsS3Object.Create(AObjectName,
                                  AResponse.ETag,
                                  AResponse.LastModified,
                                  AStream);
  finally
    AParams.Free;
    AStream.Free;
  end;
end;

function TksAwsS3.DeleteObject(ABucketName, AObjectName: string): Boolean;
begin
  Result := True;
  ExecuteHttp(C_DELETE, '', ABucketName+'.'+Host, AObjectName, nil, nil, '', True, nil);
end;

function TksAwsS3.GetServiceName: string;
begin
  Result := C_SERVICE_S3;
end;

procedure TksAwsS3.PutObject(ABucketName, APath, AFilename: string; AOptions: TksS3PutOptions; const AExtraHeaders: TStrings = nil);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFilename);
    PutObject(ABucketName, APath, AStream, AOptions, AExtraHeaders);
  finally
    AStream.Free;
  end;
end;

procedure TksAwsS3.PutObject(ABucketName, APath: string;
                             AContent: TStream;
                             AOptions: TksS3PutOptions;
                             const AExtraHeaders: TStrings = nil);
var
  AResponse: IksAwsHttpResponse;
  AHeaders: TStrings;
  ICount: integer;

begin
  AHeaders := TStringList.Create;
  try
    if Assigned(AExtraHeaders) then
    begin
      for ICount := 0 to AExtraHeaders.Count-1 do
        AHeaders.Add(AExtraHeaders.Names[ICount]+'='+AExtraHeaders.ValueFromIndex[ICount]);
    end;
    AHeaders.Values['x-amz-acl'] := GetAclString(AOptions.Acl);
    AResponse := ExecuteHttp(C_PUT, '', ABucketName+'.'+Host, APath, AHeaders, nil, AContent, True, nil);
  finally
    AHeaders.Free;
  end;
end;

end.
