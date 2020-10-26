{*******************************************************************************
*                                                                              *
*  ksAwsHttpNetClient - ksAws THttpClient Interface                            *
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

unit ksAwsHttpNetClient;

interface

uses ksAwsHttpIntf;

  function CreateksAwsHttpNetClient: IksAwsHttp;

implementation

uses Classes, Net.HttpClient, Net.UrlClient, SysUtils;

type
  TksAwsNetHttp = class(TInterfacedObject, IksAwsHttp)
  private
    function CreateHttp(AHeaders: TStrings): THTTPClient;
    procedure DoValidateCert(const Sender: TObject; const ARequest: TURLRequest;
      const Certificate: TCertificate; var Accepted: Boolean);
  protected
    function Head(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
    function Get(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
    function Put(AUrl, APayload: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
    function Post(AUrl, APayload: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
    function Delete(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): IksAwsHttpResponse;
  end;

function CreateksAwsHttpNetClient: IksAwsHttp;
begin
  Result := TksAwsNetHttp.Create;
end;

{ TksAwsNetHttp }

function ResponseToKsHttpResponse(AResponse: IHTTPResponse): IksAwsHttpResponse;
begin
  Result := CreateAwsHttpResponse;
  AResponse.ContentStream.Position := 0;
  Result.ContentStream.CopyFrom(AResponse.ContentStream, AResponse.ContentStream.Size);
  Result.StatusCode := AResponse.StatusCode;
  Result.ETag := AResponse.HeaderValue['ETag'];
  Result.LastModified := AResponse.LastModified;
end;

function TksAwsNetHttp.CreateHttp(AHeaders: TStrings): THTTPClient;
var
  ICount: integer;
begin
  Result := THTTPClient.Create;
  Result.OnValidateServerCertificate := DoValidateCert;
  for ICount := 0 to AHeaders.Count-1 do
    Result.CustomHeaders[AHeaders.Names[ICount]] := AHeaders.ValueFromIndex[ICount];
end;

function TksAwsNetHttp.Delete(AUrl: string; AHeaders: TStrings;
  const AResponseStream: TStream): IksAwsHttpResponse;
var
  AHttp: THTTPClient;
begin
  AHttp := CreateHttp(AHeaders);
  try
    Result := ResponseToKsHttpResponse(AHttp.Delete(AUrl, AResponseStream));
  finally
    AHttp.Free;
  end;
end;

procedure TksAwsNetHttp.DoValidateCert(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate;
  var Accepted: Boolean);
begin
  Accepted := True;
end;

function TksAwsNetHttp.Get(AUrl: string; AHeaders: TStrings;
  const AResponseStream: TStream): IksAwsHttpResponse;
var
  AHttp: THTTPClient;
begin
  AHttp := CreateHttp(AHeaders);
  try
    Result := ResponseToKsHttpResponse(AHttp.Get(AUrl, AResponseStream));
  finally
    AHttp.Free;
  end;
end;

function TksAwsNetHttp.Head(AUrl: string; AHeaders: TStrings;
  const AResponseStream: TStream): IksAwsHttpResponse;
var
  AHttp: THTTPClient;
begin
  AHttp := CreateHttp(AHeaders);
  try
    Result := ResponseToKsHttpResponse(AHttp.Head(AUrl));
  finally
    AHttp.Free;
  end;
end;

function TksAwsNetHttp.Post(AUrl, APayload: string; AHeaders: TStrings;
  const AResponseStream: TStream): IksAwsHttpResponse;
var
  AHttp: THTTPClient;
  AContentStream: TStringStream;
begin
  AHttp := CreateHttp(AHeaders);
  AContentStream := TStringStream.Create(APayload);
  try
    Result := ResponseToKsHttpResponse(AHttp.Post(AUrl, AContentStream, AResponseStream));
  finally
    AHttp.Free;
    AContentStream.Free;
  end;
end;

function TksAwsNetHttp.Put(AUrl, APayload: string; AHeaders: TStrings;
  const AResponseStream: TStream): IksAwsHttpResponse;
var
  AHttp: THTTPClient;
  AContentStream: TStringStream;
begin
  AHttp := CreateHttp(AHeaders);
  AContentStream := TStringStream.Create(APayload);
  try
    Result := ResponseToKsHttpResponse(AHttp.Put(AUrl, AContentStream, AResponseStream));
  finally
    AHttp.Free;
    AContentStream.Free;
  end;
end;

end.
