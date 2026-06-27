{*******************************************************************************
*                                                                              *
*  ksAwsComprehend - AWS Comprehend Interface                                  *
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

unit ksAwsComprehend;

interface

uses
  Sysutils, Classes, ksAwsBase;

type
  // One toxicity label returned for a single text segment.
  TksComprehendToxicityLabel = record
    Name: string;
    Score: Double;
  end;
  TksComprehendToxicityLabels = TArray<TksComprehendToxicityLabel>;

  // Result for one text segment. Toxicity is the overall score AWS computes
  // for the segment; Labels are the per-category scores (PROFANITY,
  // HATE_SPEECH, INSULT, etc.).
  TksComprehendToxicityResult = record
    Toxicity: Double;
    Labels: TksComprehendToxicityLabels;
  end;
  TksComprehendToxicityResults = TArray<TksComprehendToxicityResult>;

  IksAwsComprehend = interface
    ['{2B3D5F09-7C5B-4F4E-9F6D-3F90B0D5E1B0}']
    // Toxicity detection — Comprehend only supports it in us-east-1,
    // us-west-2, eu-west-1, and ap-southeast-2. Wrong region returns a
    // service error. The current NomNom call site pins eu-west-1.
    //
    // ATexts is one entry per segment; AWS returns a parallel result list.
    // ALanguageCode is the ISO 639-1 code ('en' for English).
    function DetectToxicContent(const ATexts: TArray<string>;
      const ALanguageCode: string = 'en'): TksComprehendToxicityResults;
  end;

  function CreateComprehend(AAccessKey, ASecretKey: string;
    ARegion: TksAwsRegion): IksAwsComprehend;

  // Pure parser exposed so callers (and tests) can decode a raw
  // DetectToxicContent response body without hitting the network.
  function ParseDetectToxicContentResponse(const AJson: string):
    TksComprehendToxicityResults;

implementation

uses ksAwsConst, JsonDataObjects;

const
  C_SERVICE_COMPREHEND        = 'comprehend';
  C_COMPREHEND_TARGET_PREFIX  = 'Comprehend_20171127.';
  C_COMPREHEND_CONTENT_TYPE   = 'application/x-amz-json-1.1';

type
  TksAwsComprehend = class(TksAwsBaseService, IksAwsComprehend)
  protected
    function GetServiceName: string; override;
    function DetectToxicContent(const ATexts: TArray<string>;
      const ALanguageCode: string = 'en'): TksComprehendToxicityResults;
  end;

function CreateComprehend(AAccessKey, ASecretKey: string;
  ARegion: TksAwsRegion): IksAwsComprehend;
begin
  Result := TksAwsComprehend.Create(AAccessKey, ASecretKey, ARegion);
end;

function ParseDetectToxicContentResponse(const AJson: string):
  TksComprehendToxicityResults;
var
  ARoot: TJsonObject;
  AResultList: TJsonArray;
  AResultObj, ALabelObj: TJsonObject;
  ALabelsArr: TJsonArray;
  I, J: Integer;
begin
  SetLength(Result, 0);
  if Trim(AJson) = '' then
    Exit;
  ARoot := TJsonObject.Parse(AJson) as TJsonObject;
  try
    if not ARoot.Contains('ResultList') then
      Exit;
    AResultList := ARoot.A['ResultList'];
    SetLength(Result, AResultList.Count);
    for I := 0 to AResultList.Count - 1 do
    begin
      AResultObj := AResultList.O[I];
      Result[I].Toxicity := 0.0;
      if AResultObj.Contains('Toxicity') then
        Result[I].Toxicity := AResultObj.F['Toxicity'];
      if AResultObj.Contains('Labels') then
      begin
        ALabelsArr := AResultObj.A['Labels'];
        SetLength(Result[I].Labels, ALabelsArr.Count);
        for J := 0 to ALabelsArr.Count - 1 do
        begin
          ALabelObj := ALabelsArr.O[J];
          Result[I].Labels[J].Name := ALabelObj.S['Name'];
          Result[I].Labels[J].Score := ALabelObj.F['Score'];
        end;
      end;
    end;
  finally
    ARoot.Free;
  end;
end;

{ TksAwsComprehend }

function TksAwsComprehend.GetServiceName: string;
begin
  Result := C_SERVICE_COMPREHEND;
end;

function TksAwsComprehend.DetectToxicContent(const ATexts: TArray<string>;
  const ALanguageCode: string): TksComprehendToxicityResults;
var
  ARequest: TJsonObject;
  ASegments: TJsonArray;
  ASegment: TJsonObject;
  AHeaders: TStrings;
  ABody, AResponse: string;
  I: Integer;
begin
  ARequest := TJsonObject.Create;
  AHeaders := TStringList.Create;
  try
    // Comprehend uses the AWS JSON 1.1 protocol — no Action/Version in the
    // query string; the operation goes in the X-Amz-Target header and the
    // body is the operation input as JSON. Content-Type MUST be set so the
    // SigV4 canonical request includes it; AWS will 400 otherwise.
    ASegments := ARequest.A['TextSegments'];
    for I := 0 to High(ATexts) do
    begin
      ASegment := ASegments.AddObject;
      ASegment.S['Text'] := ATexts[I];
    end;
    ARequest.S['LanguageCode'] := ALanguageCode;
    ABody := ARequest.ToJSON(True);

    AHeaders.Values['Content-Type'] := C_COMPREHEND_CONTENT_TYPE;
    AHeaders.Values['X-Amz-Target'] := C_COMPREHEND_TARGET_PREFIX +
                                       'DetectToxicContent';

    // AAction=''+ APath='/' → ExecuteHttp skips the Action/Version query
    // params (which JSON-1.1 doesn't use) but still SigV4-signs the body.
    AResponse := ExecuteHttp(C_POST, '', Host, '/', AHeaders, nil, ABody)
      .ContentAsString;
  finally
    ARequest.Free;
    AHeaders.Free;
  end;
  Result := ParseDetectToxicContentResponse(AResponse);
end;

end.
