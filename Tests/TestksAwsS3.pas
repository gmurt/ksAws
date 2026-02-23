unit TestksAwsS3;

interface

uses
  DUnitX.TestFramework, Classes, SysUtils;

type
  [TestFixture]
  TTestS3Factory = class
  public
    [Test]
    procedure TestCreateAwsS3_WithEnum;
    [Test]
    procedure TestCreateAwsS3_WithString;
    [Test]
    procedure TestCreateAwsS3_InvalidString;
    [Test]
    procedure TestCreateAwsS3_DifferentRegions;
  end;

  [TestFixture]
  TTestS3AclEnum = class
  public
    [Test]
    procedure TestAclEnumValues_AreDefined;
    [Test]
    procedure TestAclEnumValues_AreDistinct;
  end;

  [TestFixture]
  TTestS3PutOptions = class
  public
    [Test]
    procedure TestPutOptions_DefaultAcl;
    [Test]
    procedure TestPutOptions_SetAcl;
  end;

implementation

uses
  ksAwsBase, ksAwsS3;

{ TTestS3Factory }

procedure TTestS3Factory.TestCreateAwsS3_WithEnum;
var
  AService: IksAwsS3;
begin
  AService := CreateAwsS3('key', 'secret', awsEuWest2);
  Assert.IsNotNull(AService);
end;

procedure TTestS3Factory.TestCreateAwsS3_WithString;
var
  AService: IksAwsS3;
begin
  AService := CreateAwsS3('key', 'secret', 'eu-west-2');
  Assert.IsNotNull(AService);
end;

procedure TTestS3Factory.TestCreateAwsS3_InvalidString;
begin
  Assert.WillRaise(
    procedure
    begin
      CreateAwsS3('key', 'secret', 'invalid-region');
    end,
    Exception
  );
end;

procedure TTestS3Factory.TestCreateAwsS3_DifferentRegions;
var
  AService1, AService2: IksAwsS3;
begin
  AService1 := CreateAwsS3('key', 'secret', awsUsEast1);
  AService2 := CreateAwsS3('key', 'secret', awsEuWest1);
  Assert.IsNotNull(AService1);
  Assert.IsNotNull(AService2);
end;

{ TTestS3AclEnum }

procedure TTestS3AclEnum.TestAclEnumValues_AreDefined;
begin
  // Verify all four ACL values are defined and accessible
  Assert.AreEqual(0, Ord(ksS3Private));
  Assert.AreEqual(1, Ord(ksS3PublicRead));
  Assert.AreEqual(2, Ord(ksS3PublicReadWrite));
  Assert.AreEqual(3, Ord(ksS3AuthenticatedRead));
end;

procedure TTestS3AclEnum.TestAclEnumValues_AreDistinct;
begin
  Assert.AreNotEqual(Ord(ksS3Private), Ord(ksS3PublicRead));
  Assert.AreNotEqual(Ord(ksS3PublicRead), Ord(ksS3PublicReadWrite));
  Assert.AreNotEqual(Ord(ksS3PublicReadWrite), Ord(ksS3AuthenticatedRead));
end;

{ TTestS3PutOptions }

procedure TTestS3PutOptions.TestPutOptions_DefaultAcl;
var
  AOptions: TksS3PutOptions;
begin
  // Default-initialized record should have first enum value
  FillChar(AOptions, SizeOf(AOptions), 0);
  Assert.AreEqual(Ord(ksS3Private), Ord(AOptions.Acl));
end;

procedure TTestS3PutOptions.TestPutOptions_SetAcl;
var
  AOptions: TksS3PutOptions;
begin
  AOptions.Acl := ksS3PublicRead;
  Assert.AreEqual(Ord(ksS3PublicRead), Ord(AOptions.Acl));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestS3Factory);
  TDUnitX.RegisterTestFixture(TTestS3AclEnum);
  TDUnitX.RegisterTestFixture(TTestS3PutOptions);

end.
