{*******************************************************************************
*                                                                              *
*  ksAwsHttpIntf - ksAws HTTP Interface                                        *
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

unit ksAwsHttpIntf;

interface

uses Classes;

type
  TksApiHttpImplementation = (ksAwsHttpNetClient, ksAwsHttpIndy);

  IksAwsHttpResponse = interface
    ['{4703F21B-BD13-42EE-A1DC-40742F8F2469}']
    function GetContentStream: TStream;
    function GetContentAsString: string;
    function GetETag: string;
    function GetLastModified: string;
    function GetStatusCode: integer;
    procedure SetContentStream(const Value: TStream);
    procedure SetETag(const Value: string);
    procedure SetLastModified(const Value: string);
    procedure SetStatusCode(const Value: integer);
    property ContentStream: TStream read GetContentStream write SetContentStream;
    property StatusCode: integer read GetStatusCode write SetStatusCode;
    property ETag: string read GetETag write SetETag;
    property LastModified: string read GetLastModified  write SetLastModified;
    property ContentAsString: string read GetContentAsString;
  end;

  IksAwsHttp = interface
    ['{AFEEA848-BDE5-423B-8144-1DA0A7A1F036}']
    function Get(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
    function Put(AUrl, APayload: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
    function Post(AUrl, APayload: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
    function Delete(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
  end;

  function CreateAwsHttp: IksAwsHttp;
  function CreateAwsHttpResponse: IksAwsHttpResponse;

implementation

{$I include.inc}

uses
  SysUtils,
{$IFDEF USE_INDY}
  ksAwsHttpIndy;
{$ELSE}
  ksAwsHttpNetClient;
{$ENDIF}

type
  TksAwsHttpResponse = class(TInterfacedObject, IksAwsHttpResponse)
  private
    FContentStream: TStream;
    FStatusCode: integer;
    FETag: string;
    FLastModified: string;
    function GetContentStream: TStream;
    function GetETag: string;
    function GetLastModified: string;
    function GetStatusCode: integer;
    procedure SetContentStream(const Value: TStream);
    procedure SetETag(const Value: string);
    procedure SetLastModified(const Value: string);
    procedure SetStatusCode(const Value: integer);
    function GetContentAsString: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property ContentStream: TStream read GetContentStream write SetContentStream;
    property StatusCode: integer read GetStatusCode write SetStatusCode;
    property ETag: string read GetETag write SetETag;
    property LastModified: string read GetLastModified  write SetLastModified;
    property ContentAsString: string read GetContentAsString;
  end;

function CreateAwsHttp: IksAwsHttp;
begin
  {$IFDEF USE_INDY}
  Result := CreateksAwsHttpIndy;
  {$ELSE}
  Result := CreateksAwsHttpNetClient;
  {$ENDIF}
end;

function CreateAwsHttpResponse: IksAwsHttpResponse;
begin
  Result := TksAwsHttpResponse.Create;
end;

{ TksAwsHttpResponse }

constructor TksAwsHttpResponse.Create;
begin
  FContentStream := TMemoryStream.Create;
end;

destructor TksAwsHttpResponse.Destroy;
begin
  FContentStream.Free;
  inherited;
end;

function TksAwsHttpResponse.GetContentAsString: string;
var
  AStream: TStringStream;
begin
  AStream := TStringStream.Create;
  try
    FContentStream.Position := 0;
    AStream.CopyFrom(FContentStream, FContentStream.Size);
    Result := AStream.DataString;
  finally
    AStream.Free;
  end;
end;

function TksAwsHttpResponse.GetContentStream: TStream;
begin
  Result := FContentStream;
end;

function TksAwsHttpResponse.GetETag: string;
begin
  Result := FETag;
end;

function TksAwsHttpResponse.GetLastModified: string;
begin
  Result := FLastModified;
end;

function TksAwsHttpResponse.GetStatusCode: integer;
begin
  Result := FStatusCode;
end;

procedure TksAwsHttpResponse.SetContentStream(const Value: TStream);
begin
  TMemoryStream(FContentStream).Clear;
  if Value <> nil then
  begin
    Value.Position := 0;
    FContentStream.CopyFrom(Value, Value.Size);
    FContentStream.Position := 0;
  end;
end;

procedure TksAwsHttpResponse.SetETag(const Value: string);
begin
  FETag := Value;
end;

procedure TksAwsHttpResponse.SetLastModified(const Value: string);
begin
  FLastModified := Value;
end;

procedure TksAwsHttpResponse.SetStatusCode(const Value: integer);
begin
  FStatusCode := Value;
end;



end.
