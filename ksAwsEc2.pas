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
  end;

  function CreateAwsEc2(APublicKey, APrivateKey: string; ARegion: TksAwsRegion): IksAwsEc2;


implementation

uses ksAwsHttpIntf, ksAwsConst, Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc;

type
  TksAwsEC2Instance = class(TInterfacedObject, IksAwsEC2Instance)
  private
    FXml: IXmlDocument;
    FTags: TStrings;
    function GetName: string;
    function GetStatus: string;
    procedure LoadTags;
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
  end;

function CreateAwsEc2(APublicKey, APrivateKey: string; ARegion: TksAwsRegion): IksAwsEc2;
begin
  Result := TksAwsEC2.Create(APublicKey, APrivateKey, ARegion);
end;

{ TksAwsEC2Instance }

constructor TksAwsEC2Instance.Create(AXml: string);
begin
  FTags := TStringList.Create;
  FXml := TXmlDocument.Create(nil);
  FXml.LoadFromXML(AXml);
  LoadTags;
end;

destructor TksAwsEC2Instance.Destroy;
begin
  FTags.Free;
  inherited;
end;

function TksAwsEC2Instance.GetID: string;
begin
  Result := FXml.ChildNodes['item'].ChildNodes['instanceId'].Text;
end;

function TksAwsEC2Instance.GetName: string;
begin
  Result := FTags.Values['Name'];
end;

function TksAwsEC2Instance.GetStatus: string;
begin
  Result := FXml.ChildNodes['item'].ChildNodes['instanceState'].ChildNodes['name'].Text;
end;

procedure TksAwsEC2Instance.LoadTags;
var
  ICount: integer;
  ANode: IXMLNode;
  AKey: IXMLNode;
begin
  FTags.Clear;
  ANode := FXml.ChildNodes['item'].ChildNodes['tagSet'];
  for ICount := 0 to ANode.ChildNodes.Count-1 do
  begin
    AKey := ANode.ChildNodes[ICount];
    FTags.Values[AKey.ChildNodes['key'].Text] := AKey.ChildNodes['value'].Text;
  end;
end;

{ TksAwsEC2InstanceList }

procedure TksAwsEC2InstanceList.AddInstanceXml(AXml: string);
var
  AInstance: IksAwsEC2Instance;
begin
  AInstance := TksAwsEc2Instance.Create(AXml);
  Add(AInstance);
  AInstance.ID;
  AInstance.Status;
end;

{ TksAwsEC2 }

procedure TksAwsEC2.ListInstances(AInstances: TksAwsEc2InstanceList);
var
  AXml: IXMLDocument;
  AResponse: IXMLNode;
  AItems: IXMLNode;
  AItem: IXMLNode;
  ICount: integer;
begin
  AInstances.Clear;
  AXml := ExecuteHttpXml(C_GET, 'DescribeInstances', Host, '', '', nil, nil, True, nil);
  AResponse := AXml.ChildNodes['DescribeInstancesResponse'];
  AItems := AResponse.ChildNodes['reservationSet'];
  for ICount := 0 to AItems.ChildNodes.Count-1 do
  begin
    AItem := AItems.ChildNodes[ICount].ChildNodes['instancesSet'].ChildNodes['item'];
    AInstances.AddInstanceXml(AItem.XML);
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
    ExecuteHttpXml(C_GET, 'StartInstances', Host, '', '', nil, AParams, True, nil);
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
    ExecuteHttpXml(C_GET, 'StopInstances', Host, '', '', nil, AParams, True, nil);
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


