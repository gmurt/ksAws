unit TestksAwsRDS;

interface

uses
  DUnitX.TestFramework, Classes, SysUtils;

type
  [TestFixture]
  TTestRDSInstanceXmlParsing = class
  public
    [Test]
    procedure TestParseName;
    [Test]
    procedure TestParseStatus_Available;
    [Test]
    procedure TestParseStatus_Stopped;
  end;

  [TestFixture]
  TTestRDSInstanceList = class
  public
    [Test]
    procedure TestAddInstanceXml_IncreasesCount;
    [Test]
    procedure TestAddMultipleInstances;
    [Test]
    procedure TestClearList;
  end;

  [TestFixture]
  TTestRDSFactory = class
  public
    [Test]
    procedure TestCreateAwsRDS_ReturnsValidInterface;
    [Test]
    procedure TestInstanceList_Create;
  end;

implementation

uses
  ksAwsBase, ksAwsRDS;

const
  C_RDS_INSTANCE_XML =
    '<DBInstance>' +
    '<DBInstanceIdentifier>my-database</DBInstanceIdentifier>' +
    '<DBInstanceStatus>available</DBInstanceStatus>' +
    '</DBInstance>';

  C_RDS_INSTANCE_XML_STOPPED =
    '<DBInstance>' +
    '<DBInstanceIdentifier>test-db-01</DBInstanceIdentifier>' +
    '<DBInstanceStatus>stopped</DBInstanceStatus>' +
    '</DBInstance>';

{ TTestRDSInstanceXmlParsing }

procedure TTestRDSInstanceXmlParsing.TestParseName;
var
  AList: TksAwsRDSInstanceList;
begin
  AList := TksAwsRDSInstanceList.Create;
  try
    AList.AddInstanceXml(C_RDS_INSTANCE_XML);
    Assert.AreEqual('my-database', AList[0].Name);
  finally
    AList.Free;
  end;
end;

procedure TTestRDSInstanceXmlParsing.TestParseStatus_Available;
var
  AList: TksAwsRDSInstanceList;
begin
  AList := TksAwsRDSInstanceList.Create;
  try
    AList.AddInstanceXml(C_RDS_INSTANCE_XML);
    Assert.AreEqual('available', AList[0].Status);
  finally
    AList.Free;
  end;
end;

procedure TTestRDSInstanceXmlParsing.TestParseStatus_Stopped;
var
  AList: TksAwsRDSInstanceList;
begin
  AList := TksAwsRDSInstanceList.Create;
  try
    AList.AddInstanceXml(C_RDS_INSTANCE_XML_STOPPED);
    Assert.AreEqual('stopped', AList[0].Status);
  finally
    AList.Free;
  end;
end;

{ TTestRDSInstanceList }

procedure TTestRDSInstanceList.TestAddInstanceXml_IncreasesCount;
var
  AList: TksAwsRDSInstanceList;
begin
  AList := TksAwsRDSInstanceList.Create;
  try
    Assert.AreEqual(0, AList.Count);
    AList.AddInstanceXml(C_RDS_INSTANCE_XML);
    Assert.AreEqual(1, AList.Count);
  finally
    AList.Free;
  end;
end;

procedure TTestRDSInstanceList.TestAddMultipleInstances;
var
  AList: TksAwsRDSInstanceList;
begin
  AList := TksAwsRDSInstanceList.Create;
  try
    AList.AddInstanceXml(C_RDS_INSTANCE_XML);
    AList.AddInstanceXml(C_RDS_INSTANCE_XML_STOPPED);
    Assert.AreEqual(2, AList.Count);
  finally
    AList.Free;
  end;
end;

procedure TTestRDSInstanceList.TestClearList;
var
  AList: TksAwsRDSInstanceList;
begin
  AList := TksAwsRDSInstanceList.Create;
  try
    AList.AddInstanceXml(C_RDS_INSTANCE_XML);
    AList.AddInstanceXml(C_RDS_INSTANCE_XML_STOPPED);
    AList.Clear;
    Assert.AreEqual(0, AList.Count);
  finally
    AList.Free;
  end;
end;

{ TTestRDSFactory }

procedure TTestRDSFactory.TestCreateAwsRDS_ReturnsValidInterface;
var
  AService: IksAwsRDS;
begin
  AService := CreateAwsRDS('key', 'secret', awsUsEast1);
  Assert.IsNotNull(AService);
end;

procedure TTestRDSFactory.TestInstanceList_Create;
var
  AList: TksAwsRDSInstanceList;
begin
  AList := TksAwsRDSInstanceList.Create;
  try
    Assert.AreEqual(0, AList.Count);
  finally
    AList.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestRDSInstanceXmlParsing);
  TDUnitX.RegisterTestFixture(TTestRDSInstanceList);
  TDUnitX.RegisterTestFixture(TTestRDSFactory);

end.
