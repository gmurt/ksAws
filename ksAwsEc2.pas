{*******************************************************************************
*                                                                              *
*  ksEC2 - AWS EC2 Interface                                                   *
*                                                                              *
*  https://github.com/gmurt/ksAWS                                            *
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

unit ksAwsEc2;

interface

uses
  Sysutils, Classes, System.Generics.Collections, ksAwsBase;

type
  IksAwsEC2Instance = interface
    ['{8C20BBCB-262E-492E-8A47-CDDF168B5576}']
    function GetID: string;
    function GetName: string;
    function GetStatus: string;
    property ID: string read GetID;
    property Name: string read GetName;
    property Status: string read GetStatus;
  end;

  TksAwsEC2InstanceList = class(TList<IksAwsEC2Instance>)
  public
    procedure AddInstanceXml(AXml: string);
  end;

  IksAwsEC2 = interface
    ['{ADF72B95-53AA-4983-A5A5-4E532AD95E50}']
    procedure ListInstances(AInstances: TksAwsEc2InstanceList);
    procedure StartInstances(AInstanceIDs: array of string);
    procedure StopInstances(AInstanceIDs: array of string);
    // Security-group ingress management. DescribeSecurityGroupRules returns the
    // raw DescribeSecurityGroups XML for AGroupId (caller parses it). Authorize
    // adds one inbound rule using the nested IpPermissions form so the optional
    // rule Description is applied; Revoke removes a rule, matched by AWS on
    // protocol + port range + CIDR (description is not part of the match).
    function DescribeSecurityGroupRules(AGroupId: string): string;
    procedure AuthorizeSecurityGroupIngress(AGroupId, AProtocol: string;
      AFromPort, AToPort: Integer; ACidr, ADescription: string);
    procedure RevokeSecurityGroupIngress(AGroupId, AProtocol: string;
      AFromPort, AToPort: Integer; ACidr: string);
  end;

  function CreateAwsEc2(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion): IksAwsEc2;


implementation

uses ksAwsHttpIntf, ksAwsConst, ksAwsXml;

type
  TksAwsEC2Instance = class(TInterfacedObject, IksAwsEC2Instance)
  private
    FId: string;
    FStatus: string;
    FTags: TStrings;
    function GetName: string;
    function GetStatus: string;
    function GetID: string;
  protected
    property ID: string read GetID;
    property Name: string read GetName;
    property Status: string read GetStatus;
  public
    constructor Create(AXml: string);
    destructor Destroy; override;
  end;

  TksAwsEC2 = class(TksAwsBaseService, IksAwsEc2)
  private
    { Private declarations }
  protected
    function GetApiVersion: string; override;
    function GetServiceName: string; override;
    procedure ListInstances(AInstances: TksAwsEc2InstanceList);
    procedure StartInstances(AInstanceIDs: array of string);
    procedure StopInstances(AInstanceIDs: array of string);
    function DescribeSecurityGroupRules(AGroupId: string): string;
    procedure AuthorizeSecurityGroupIngress(AGroupId, AProtocol: string;
      AFromPort, AToPort: Integer; ACidr, ADescription: string);
    procedure RevokeSecurityGroupIngress(AGroupId, AProtocol: string;
      AFromPort, AToPort: Integer; ACidr: string);
  end;

function CreateAwsEc2(AAccessKey, ASecretKey: string; ARegion: TksAwsRegion): IksAwsEc2;
begin
  Result := TksAwsEC2.Create(AAccessKey, ASecretKey, ARegion);
end;

{ TksAwsEC2Instance }

constructor TksAwsEC2Instance.Create(AXml: string);
var
  ATagSet, ABlock, AKey, AValue: string;
  AOffset, ABlockEnd: integer;
begin
  FTags := TStringList.Create;
  FId := GetXmlTagValue(AXml, 'instanceId');
  FStatus := GetXmlTagValue(GetXmlTagValue(AXml, 'instanceState'), 'name');
  ATagSet := GetXmlTagValue(AXml, 'tagSet');
  AOffset := 1;
  while True do
  begin
    ABlock := GetXmlBlock(ATagSet, 'item', AOffset, ABlockEnd);
    if ABlockEnd = 0 then Break;
    AKey := GetXmlTagValue(ABlock, 'key');
    AValue := GetXmlTagValue(ABlock, 'value');
    FTags.Values[AKey] := AValue;
    AOffset := ABlockEnd;
  end;
end;

destructor TksAwsEC2Instance.Destroy;
begin
  FTags.Free;
  inherited;
end;

function TksAwsEC2Instance.GetID: string;
begin
  Result := FId;
end;

function TksAwsEC2Instance.GetName: string;
begin
  Result := FTags.Values['Name'];
end;

function TksAwsEC2Instance.GetStatus: string;
begin
  Result := FStatus;
end;

{ TksAwsEC2InstanceList }

procedure TksAwsEC2InstanceList.AddInstanceXml(AXml: string);
var
  AInstance: IksAwsEC2Instance;
begin
  AInstance := TksAwsEc2Instance.Create(AXml);
  Add(AInstance);
end;

{ TksAwsEC2 }

procedure TksAwsEC2.ListInstances(AInstances: TksAwsEc2InstanceList);
var
  AXmlStr, AReservationSet, AReservation, AInstancesSet, AInstanceXml: string;
  AOffset, ABlockEnd, AOffset2, ABlockEnd2: integer;
begin
  AInstances.Clear;
  AXmlStr := ExecuteHttp(C_GET, 'DescribeInstances', Host, '', nil, nil, '', True, nil).ContentAsString;
  AReservationSet := GetXmlTagValue(AXmlStr, 'reservationSet');
  AOffset := 1;
  while True do
  begin
    AReservation := GetXmlBlock(AReservationSet, 'item', AOffset, ABlockEnd);
    if ABlockEnd = 0 then Break;
    AInstancesSet := GetXmlTagValue(AReservation, 'instancesSet');
    AOffset2 := 1;
    while True do
    begin
      AInstanceXml := GetXmlBlock(AInstancesSet, 'item', AOffset2, ABlockEnd2);
      if ABlockEnd2 = 0 then Break;
      AInstances.AddInstanceXml('<item>' + AInstanceXml + '</item>');
      AOffset2 := ABlockEnd2;
    end;
    AOffset := ABlockEnd;
  end;
end;

procedure TksAwsEC2.StartInstances(AInstanceIDs: array of string);
var
  ICount: integer;
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    for ICount := Low(AInstanceIDs) to High(AInstanceIDs) do
      AParams.Values['InstanceId.'+IntToStr(ICount+1)] := AInstanceIDs[ICount];
    ExecuteHttp(C_GET, 'StartInstances', Host, '', nil, AParams, '', True, nil);
  finally
    AParams.Free;
  end;
end;

procedure TksAwsEC2.StopInstances(AInstanceIDs: array of string);
var
  ICount: integer;
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    for ICount := Low(AInstanceIDs) to High(AInstanceIDs) do
      AParams.Values['InstanceId.'+IntToStr(ICount+1)] := AInstanceIDs[ICount];
    ExecuteHttp(C_GET, 'StopInstances', Host, '', nil, AParams, '', True, nil);
  finally
    AParams.Free;
  end;
end;

function TksAwsEC2.DescribeSecurityGroupRules(AGroupId: string): string;
var
  AParams: TStrings;
begin
  // Classic DescribeSecurityGroups action, filtered to a single group via the
  // GroupId.1 parameter. The XML body (securityGroupInfo > item > ipPermissions
  // > item > ipRanges > item > cidrIp/description) is returned verbatim for the
  // caller to parse.
  AParams := TStringList.Create;
  try
    AParams.Values['GroupId.1'] := AGroupId;
    Result := ExecuteHttp(C_GET, 'DescribeSecurityGroups', Host, '', nil, AParams, '', True, nil).ContentAsString;
  finally
    AParams.Free;
  end;
end;

procedure TksAwsEC2.AuthorizeSecurityGroupIngress(AGroupId, AProtocol: string;
  AFromPort, AToPort: Integer; ACidr, ADescription: string);
var
  AParams: TStrings;
begin
  // Nested IpPermissions form: this is the ONLY form that lets us attach a
  // Description to the CIDR range. The library URL-encodes both the dotted
  // parameter names and the CIDR slash (-> %2F) consistently in the signed
  // canonical request and on the wire, so values are passed raw here.
  AParams := TStringList.Create;
  try
    AParams.Values['GroupId'] := AGroupId;
    AParams.Values['IpPermissions.1.IpProtocol'] := AProtocol;
    AParams.Values['IpPermissions.1.FromPort'] := IntToStr(AFromPort);
    AParams.Values['IpPermissions.1.ToPort'] := IntToStr(AToPort);
    AParams.Values['IpPermissions.1.IpRanges.1.CidrIp'] := ACidr;
    if ADescription <> '' then
      AParams.Values['IpPermissions.1.IpRanges.1.Description'] := ADescription;
    ExecuteHttp(C_GET, 'AuthorizeSecurityGroupIngress', Host, '', nil, AParams, '', True, nil);
  finally
    AParams.Free;
  end;
end;

procedure TksAwsEC2.RevokeSecurityGroupIngress(AGroupId, AProtocol: string;
  AFromPort, AToPort: Integer; ACidr: string);
var
  AParams: TStrings;
begin
  // Flat form: AWS matches the rule to revoke on protocol + port range + CIDR
  // (the description is ignored on revoke), so the simpler top-level
  // parameters are sufficient and reliable.
  AParams := TStringList.Create;
  try
    AParams.Values['GroupId'] := AGroupId;
    AParams.Values['IpProtocol'] := AProtocol;
    AParams.Values['FromPort'] := IntToStr(AFromPort);
    AParams.Values['ToPort'] := IntToStr(AToPort);
    AParams.Values['CidrIp'] := ACidr;
    ExecuteHttp(C_GET, 'RevokeSecurityGroupIngress', Host, '', nil, AParams, '', True, nil);
  finally
    AParams.Free;
  end;
end;

function TksAwsEC2.GetApiVersion: string;
begin
  Result := C_EC2_API_VERSION;
end;

function TksAwsEC2.GetServiceName: string;
begin
  Result := C_SERVICE_EC2;
end;



end.


