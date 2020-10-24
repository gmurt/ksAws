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

{.$DEFINE USE_INDY_HTTP}

type
  TksApiHttpImplementation = (ksAwsHttpNetClient, ksAwsHttpIndy);

  TksAwsHttpResponse = record
    ContentText: string;
    StatusCode: integer;
    ETag: string;
    LastModified: string;
  end;

  IksAwsHttp = interface
    ['{AFEEA848-BDE5-423B-8144-1DA0A7A1F036}']
    function Get(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): TksAwsHttpResponse;
    function Put(AUrl, APayload: string; AHeaders: TStrings; const AResponseStream: TStream = nil): TksAwsHttpResponse;
    function Post(AUrl, APayload: string; AHeaders: TStrings; const AResponseStream: TStream = nil): TksAwsHttpResponse;
    function Delete(AUrl: string; AHeaders: TStrings; const AResponseStream: TStream = nil): TksAwsHttpResponse;
  end;

  function CreateAwsHttp: IksAwsHttp;

implementation

{$IFDEF USE_INDY_HTTP}
uses
  ksAwsHttpIndy;
{$ELSE}
uses
  ksAwsHttpNetClient;
{$ENDIF}


function CreateAwsHttp: IksAwsHttp;
begin
  {$IFDEF USE_INDY_HTTP}
  Result := CreateksAwsHttpIndy;
  {$ELSE}
  Result := CreateksAwsHttpNetClient;
  {$ENDIF}
end;


end.
