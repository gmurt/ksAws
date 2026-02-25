unit TestksAwsBase;

interface

uses
  DUnitX.TestFramework, Classes, SysUtils;

type
  [TestFixture]
  TTestStringToRegion = class
  public
    [Test]
    procedure TestEuWest1;
    [Test]
    procedure TestEuWest2;
    [Test]
    procedure TestEuWest3;
    [Test]
    procedure TestEuCentral1;
    [Test]
    procedure TestEuNorth1;
    [Test]
    procedure TestEuSouth1;
    [Test]
    procedure TestUsEast1;
    [Test]
    procedure TestUsEast2;
    [Test]
    procedure TestUsWest1;
    [Test]
    procedure TestUsWest2;
    [Test]
    procedure TestUpperCaseInput;
    [Test]
    procedure TestMixedCaseInput;
    [Test]
    procedure TestWhitespaceInput;
    [Test]
    procedure TestInvalidRegion;
  end;

  [TestFixture]
  TTestBaseServiceProperties = class
  public
    [Test]
    procedure TestRegionStr_EuWest2;
    [Test]
    procedure TestRegionStr_UsEast1;
    [Test]
    procedure TestAccessKey;
    [Test]
    procedure TestSecretKey;
    [Test]
    procedure TestHost;
    [Test]
    procedure TestServiceName;
  end;

  [TestFixture]
  TTestCanonicalRequest = class
  public
    [Test]
    procedure TestGetRequest_EmptyPayload;
    [Test]
    procedure TestGetRequest_WithQueryParams;
    [Test]
    procedure TestPostRequest_WithPayload;
    [Test]
    procedure TestCanonical_HeadersAreSorted;
    [Test]
    procedure TestCanonical_HeaderNamesLowerCased;
    [Test]
    procedure TestCanonical_QueryParamsSorted;
    [Test]
    procedure TestCanonical_VerbNotEncoded;
    [Test]
    procedure TestCanonical_PayloadHashIncluded;
  end;

  [TestFixture]
  TTestAuthHeaderFormat = class
  public
    [Test]
    procedure TestAuthHeader_ContainsAlgorithm;
    [Test]
    procedure TestAuthHeader_ContainsCredential;
    [Test]
    procedure TestAuthHeader_ContainsSignedHeaders;
    [Test]
    procedure TestAuthHeader_ContainsSignature;
    [Test]
    procedure TestAuthHeader_SpaceAfterCommas;
  end;

  [TestFixture]
  TTestCanonicalRequestContentType = class
  public
    [Test]
    procedure TestCanonical_ContentTypeHeaderSigned;
    [Test]
    procedure TestCanonical_ContentTypeHeaderLowerCased;
    [Test]
    procedure TestCanonical_ContentTypeInSignedHeadersList;
    [Test]
    procedure TestCanonical_NoQueryString_EmptyLine;
  end;

implementation

uses
  ksAwsBase, ksAwsConst, ksAwsHash, DateUtils;

type
  // Test subclass to expose protected methods for testing
  TTestAwsService = class(TksAwsBaseService)
  protected
    function GetServiceName: string; override;
    function GetApiVersion: string; override;
  public
    function TestGenerateCanonicalRequest(AVerb, URI: string;
      APayload: TStream; AHeaders, AQueryValues: TStrings): string;
    function TestGetHost: string;
  end;

function TTestAwsService.GetServiceName: string;
begin
  Result := 'testservice';
end;

function TTestAwsService.GetApiVersion: string;
begin
  Result := '2020-01-01';
end;

function TTestAwsService.TestGenerateCanonicalRequest(AVerb, URI: string;
  APayload: TStream; AHeaders, AQueryValues: TStrings): string;
begin
  Result := GenerateCanonicalRequest(AVerb, URI, APayload, AHeaders, AQueryValues);
end;

function TTestAwsService.TestGetHost: string;
begin
  Result := GetHost;
end;

{ TTestStringToRegion }

procedure TTestStringToRegion.TestEuWest1;
begin
  Assert.AreEqual(Ord(awsEuWest1), Ord(StringToRegion('eu-west-1')));
end;

procedure TTestStringToRegion.TestEuWest2;
begin
  Assert.AreEqual(Ord(awsEuWest2), Ord(StringToRegion('eu-west-2')));
end;

procedure TTestStringToRegion.TestEuWest3;
begin
  Assert.AreEqual(Ord(awsEuWest3), Ord(StringToRegion('eu-west-3')));
end;

procedure TTestStringToRegion.TestEuCentral1;
begin
  Assert.AreEqual(Ord(awsEuCentral1), Ord(StringToRegion('eu-central-1')));
end;

procedure TTestStringToRegion.TestEuNorth1;
begin
  Assert.AreEqual(Ord(awsEuNorth1), Ord(StringToRegion('eu-north-1')));
end;

procedure TTestStringToRegion.TestEuSouth1;
begin
  Assert.AreEqual(Ord(awsEuSouth1), Ord(StringToRegion('eu-south-1')));
end;

procedure TTestStringToRegion.TestUsEast1;
begin
  Assert.AreEqual(Ord(awsUsEast1), Ord(StringToRegion('us-east-1')));
end;

procedure TTestStringToRegion.TestUsEast2;
begin
  Assert.AreEqual(Ord(awsUsEast2), Ord(StringToRegion('us-east-2')));
end;

procedure TTestStringToRegion.TestUsWest1;
begin
  Assert.AreEqual(Ord(awsUsWest1), Ord(StringToRegion('us-west-1')));
end;

procedure TTestStringToRegion.TestUsWest2;
begin
  Assert.AreEqual(Ord(awsUsWest2), Ord(StringToRegion('us-west-2')));
end;

procedure TTestStringToRegion.TestUpperCaseInput;
begin
  Assert.AreEqual(Ord(awsEuWest2), Ord(StringToRegion('EU-WEST-2')));
end;

procedure TTestStringToRegion.TestMixedCaseInput;
begin
  Assert.AreEqual(Ord(awsUsEast1), Ord(StringToRegion('Us-East-1')));
end;

procedure TTestStringToRegion.TestWhitespaceInput;
begin
  Assert.AreEqual(Ord(awsUsWest2), Ord(StringToRegion('  us-west-2  ')));
end;

procedure TTestStringToRegion.TestInvalidRegion;
begin
  Assert.WillRaise(
    procedure
    begin
      StringToRegion('invalid-region');
    end,
    Exception
  );
end;

{ TTestBaseServiceProperties }

procedure TTestBaseServiceProperties.TestRegionStr_EuWest2;
var
  AService: TTestAwsService;
begin
  AService := TTestAwsService.Create('key', 'secret', awsEuWest2);
  try
    Assert.AreEqual('eu-west-2', AService.RegionStr);
  finally
    AService.Free;
  end;
end;

procedure TTestBaseServiceProperties.TestRegionStr_UsEast1;
var
  AService: TTestAwsService;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  try
    Assert.AreEqual('us-east-1', AService.RegionStr);
  finally
    AService.Free;
  end;
end;

procedure TTestBaseServiceProperties.TestAccessKey;
var
  AService: TTestAwsService;
begin
  AService := TTestAwsService.Create('AKIAIOSFODNN7EXAMPLE', 'secret', awsEuWest2);
  try
    Assert.AreEqual('AKIAIOSFODNN7EXAMPLE', AService.AccessKey);
  finally
    AService.Free;
  end;
end;

procedure TTestBaseServiceProperties.TestSecretKey;
var
  AService: TTestAwsService;
begin
  AService := TTestAwsService.Create('key', 'wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY', awsEuWest2);
  try
    Assert.AreEqual('wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY', AService.SecretKey);
  finally
    AService.Free;
  end;
end;

procedure TTestBaseServiceProperties.TestHost;
var
  AService: TTestAwsService;
begin
  AService := TTestAwsService.Create('key', 'secret', awsEuWest2);
  try
    Assert.AreEqual('testservice.eu-west-2.amazonaws.com', AService.TestGetHost);
  finally
    AService.Free;
  end;
end;

procedure TTestBaseServiceProperties.TestServiceName;
var
  AService: TTestAwsService;
begin
  AService := TTestAwsService.Create('key', 'secret', awsEuWest2);
  try
    Assert.AreEqual('testservice', AService.ServiceName);
  finally
    AService.Free;
  end;
end;

{ TTestCanonicalRequest }

procedure TTestCanonicalRequest.TestGetRequest_EmptyPayload;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AStream: TStringStream;
  AResult: string;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AStream := TStringStream.Create('', TEncoding.UTF8);
  try
    AHeaders.Values['host'] := 'testservice.us-east-1.amazonaws.com';
    AHeaders.Values['x-amz-date'] := '20150830T123600Z';
    AHeaders.Values['x-amz-content-sha256'] := C_EMPTY_HASH;
    AResult := AService.TestGenerateCanonicalRequest('GET', '/', AStream, AHeaders, nil);

    // Should start with GET
    Assert.StartsWith('GET' + #10, AResult);
    // Should contain the empty hash for the payload
    Assert.EndsWith(C_EMPTY_HASH, AResult);
  finally
    AStream.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

procedure TTestCanonicalRequest.TestGetRequest_WithQueryParams;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AParams: TStringList;
  AStream: TStringStream;
  AResult: string;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AParams := TStringList.Create;
  AStream := TStringStream.Create('', TEncoding.UTF8);
  try
    AHeaders.Values['host'] := 'testservice.us-east-1.amazonaws.com';
    AHeaders.Values['x-amz-date'] := '20150830T123600Z';
    AHeaders.Values['x-amz-content-sha256'] := C_EMPTY_HASH;
    AParams.Values['Action'] := 'ListUsers';
    AParams.Values['Version'] := '2010-05-08';

    AResult := AService.TestGenerateCanonicalRequest('GET', '/', AStream, AHeaders, AParams);
    Assert.Contains(AResult, 'Action=ListUsers');
    Assert.Contains(AResult, 'Version=2010-05-08');
  finally
    AStream.Free;
    AParams.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

procedure TTestCanonicalRequest.TestPostRequest_WithPayload;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AStream: TStringStream;
  AResult: string;
  APayloadHash: string;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AStream := TStringStream.Create('test-payload', TEncoding.UTF8);
  try
    APayloadHash := GetHashSHA256Hex('test-payload');
    AHeaders.Values['host'] := 'testservice.us-east-1.amazonaws.com';
    AHeaders.Values['x-amz-date'] := '20150830T123600Z';
    AHeaders.Values['x-amz-content-sha256'] := APayloadHash;

    AResult := AService.TestGenerateCanonicalRequest('POST', '/', AStream, AHeaders, nil);
    Assert.StartsWith('POST' + #10, AResult);
    Assert.EndsWith(APayloadHash, AResult);
  finally
    AStream.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

procedure TTestCanonicalRequest.TestCanonical_HeadersAreSorted;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AStream: TStringStream;
  AResult: string;
  AHostPos, ADatePos: integer;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AStream := TStringStream.Create('', TEncoding.UTF8);
  try
    // Add headers in reverse order - canonical request should still have them sorted
    AHeaders.Values['x-amz-date'] := '20150830T123600Z';
    AHeaders.Values['host'] := 'testservice.us-east-1.amazonaws.com';
    AHeaders.Values['x-amz-content-sha256'] := C_EMPTY_HASH;
    (AHeaders as TStringList).Sort;

    AResult := AService.TestGenerateCanonicalRequest('GET', '/', AStream, AHeaders, nil);
    AHostPos := Pos('host:', AResult);
    ADatePos := Pos('x-amz-date:', AResult);
    Assert.IsTrue(AHostPos < ADatePos, 'host header should appear before x-amz-date');
  finally
    AStream.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

procedure TTestCanonicalRequest.TestCanonical_HeaderNamesLowerCased;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AStream: TStringStream;
  AResult: string;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AStream := TStringStream.Create('', TEncoding.UTF8);
  try
    // Use mixed-case header names
    AHeaders.Values['Host'] := 'testservice.us-east-1.amazonaws.com';
    AHeaders.Values['X-Amz-Date'] := '20150830T123600Z';
    AHeaders.Values['X-Amz-Content-Sha256'] := C_EMPTY_HASH;

    AResult := AService.TestGenerateCanonicalRequest('GET', '/', AStream, AHeaders, nil);
    // Canonical request should contain lowercase header names
    Assert.Contains(AResult, 'host:');
    Assert.Contains(AResult, 'x-amz-date:');
    Assert.Contains(AResult, 'x-amz-content-sha256:');
    // Verify no uppercase header names survive (case-sensitive check)
    Assert.AreEqual(0, Pos('Host:', AResult), 'Host: should be lowercased to host:');
    Assert.AreEqual(0, Pos('X-Amz-Date:', AResult), 'X-Amz-Date: should be lowercased');
  finally
    AStream.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

procedure TTestCanonicalRequest.TestCanonical_QueryParamsSorted;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AParams: TStringList;
  AStream: TStringStream;
  AResult: string;
  AActionPos, AVersionPos: integer;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AParams := TStringList.Create;
  AStream := TStringStream.Create('', TEncoding.UTF8);
  try
    AHeaders.Values['host'] := 'testservice.us-east-1.amazonaws.com';
    AHeaders.Values['x-amz-date'] := '20150830T123600Z';
    AHeaders.Values['x-amz-content-sha256'] := C_EMPTY_HASH;
    // Add in reverse alphabetical order
    AParams.Values['Version'] := '2010-05-08';
    AParams.Values['Action'] := 'ListUsers';

    AResult := AService.TestGenerateCanonicalRequest('GET', '/', AStream, AHeaders, AParams);
    AActionPos := Pos('Action=', AResult);
    AVersionPos := Pos('Version=', AResult);
    Assert.IsTrue(AActionPos < AVersionPos, 'Action should appear before Version (sorted)');
  finally
    AStream.Free;
    AParams.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

procedure TTestCanonicalRequest.TestCanonical_VerbNotEncoded;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AStream: TStringStream;
  AResult: string;
  AFirstLine: string;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AStream := TStringStream.Create('', TEncoding.UTF8);
  try
    AHeaders.Values['host'] := 'testservice.us-east-1.amazonaws.com';
    AHeaders.Values['x-amz-date'] := '20150830T123600Z';
    AHeaders.Values['x-amz-content-sha256'] := C_EMPTY_HASH;

    AResult := AService.TestGenerateCanonicalRequest('GET', '/', AStream, AHeaders, nil);
    AFirstLine := Copy(AResult, 1, Pos(#10, AResult) - 1);
    Assert.AreEqual('GET', AFirstLine);
  finally
    AStream.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

procedure TTestCanonicalRequest.TestCanonical_PayloadHashIncluded;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AStream: TStringStream;
  AResult: string;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AStream := TStringStream.Create('', TEncoding.UTF8);
  try
    AHeaders.Values['host'] := 'testservice.us-east-1.amazonaws.com';
    AHeaders.Values['x-amz-date'] := '20150830T123600Z';
    AHeaders.Values['x-amz-content-sha256'] := C_EMPTY_HASH;

    AResult := AService.TestGenerateCanonicalRequest('GET', '/', AStream, AHeaders, nil);
    Assert.EndsWith(C_EMPTY_HASH, AResult);
  finally
    AStream.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

{ TTestAuthHeaderFormat }

procedure TTestAuthHeaderFormat.TestAuthHeader_ContainsAlgorithm;
var
  AHeader: string;
begin
  AHeader := C_HASH_ALGORITHM + ' Credential=AKIDEXAMPLE/20150830/us-east-1/iam/aws4_request, SignedHeaders=host;x-amz-date, Signature=abc123';
  Assert.StartsWith(C_HASH_ALGORITHM, AHeader);
end;

procedure TTestAuthHeaderFormat.TestAuthHeader_ContainsCredential;
var
  AHeader: string;
begin
  AHeader := C_HASH_ALGORITHM + ' Credential=AKIDEXAMPLE/20150830/us-east-1/iam/aws4_request, SignedHeaders=host;x-amz-date, Signature=abc123';
  Assert.Contains(AHeader, 'Credential=AKIDEXAMPLE');
end;

procedure TTestAuthHeaderFormat.TestAuthHeader_ContainsSignedHeaders;
var
  AHeader: string;
begin
  AHeader := C_HASH_ALGORITHM + ' Credential=AKIDEXAMPLE/20150830/us-east-1/iam/aws4_request, SignedHeaders=host;x-amz-date, Signature=abc123';
  Assert.Contains(AHeader, 'SignedHeaders=host;x-amz-date');
end;

procedure TTestAuthHeaderFormat.TestAuthHeader_ContainsSignature;
var
  AHeader: string;
begin
  AHeader := C_HASH_ALGORITHM + ' Credential=AKIDEXAMPLE/20150830/us-east-1/iam/aws4_request, SignedHeaders=host;x-amz-date, Signature=abc123';
  Assert.Contains(AHeader, 'Signature=abc123');
end;

procedure TTestAuthHeaderFormat.TestAuthHeader_SpaceAfterCommas;
var
  AHeader: string;
begin
  // Verify the AWS-spec format has spaces after commas
  AHeader := C_HASH_ALGORITHM + ' Credential=AKIDEXAMPLE/20150830/us-east-1/iam/aws4_request, SignedHeaders=host;x-amz-date, Signature=abc123';
  Assert.Contains(AHeader, 'aws4_request, SignedHeaders=');
  Assert.Contains(AHeader, 'x-amz-date, Signature=');
  // Ensure no missing spaces (comma immediately followed by letter)
  Assert.DoesNotContain(AHeader, 'aws4_request,SignedHeaders');
  Assert.DoesNotContain(AHeader, 'x-amz-date,Signature');
end;

{ TTestCanonicalRequestContentType }

procedure TTestCanonicalRequestContentType.TestCanonical_ContentTypeHeaderSigned;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AStream: TStringStream;
  AResult: string;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AStream := TStringStream.Create('{"test":"data"}', TEncoding.UTF8);
  try
    AHeaders.Values['content-type'] := 'application/json';
    AHeaders.Values['host'] := 'email.us-east-1.amazonaws.com';
    AHeaders.Values['x-amz-date'] := '20250101T120000Z';
    AHeaders.Values['x-amz-content-sha256'] := GetHashSHA256Hex('{"test":"data"}');
    (AHeaders as TStringList).Sort;

    AResult := AService.TestGenerateCanonicalRequest('POST', '/v2/email/outbound-emails', AStream, AHeaders, nil);
    // content-type header should appear in the canonical headers section
    Assert.Contains(AResult, 'content-type:application/json');
  finally
    AStream.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

procedure TTestCanonicalRequestContentType.TestCanonical_ContentTypeHeaderLowerCased;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AStream: TStringStream;
  AResult: string;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AStream := TStringStream.Create('{}', TEncoding.UTF8);
  try
    // Pass Content-Type with mixed case - canonical request should lowercase it
    AHeaders.Values['Content-Type'] := 'application/json';
    AHeaders.Values['host'] := 'email.us-east-1.amazonaws.com';
    AHeaders.Values['x-amz-date'] := '20250101T120000Z';
    AHeaders.Values['x-amz-content-sha256'] := GetHashSHA256Hex('{}');
    (AHeaders as TStringList).Sort;

    AResult := AService.TestGenerateCanonicalRequest('POST', '/v2/email/outbound-emails', AStream, AHeaders, nil);
    Assert.Contains(AResult, 'content-type:application/json');
    // Should not contain the mixed-case version
    Assert.AreEqual(0, Pos('Content-Type:', AResult));
  finally
    AStream.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

procedure TTestCanonicalRequestContentType.TestCanonical_ContentTypeInSignedHeadersList;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AStream: TStringStream;
  AResult: string;
begin
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AStream := TStringStream.Create('{}', TEncoding.UTF8);
  try
    AHeaders.Values['content-type'] := 'application/json';
    AHeaders.Values['host'] := 'email.us-east-1.amazonaws.com';
    AHeaders.Values['x-amz-date'] := '20250101T120000Z';
    AHeaders.Values['x-amz-content-sha256'] := GetHashSHA256Hex('{}');
    (AHeaders as TStringList).Sort;

    AResult := AService.TestGenerateCanonicalRequest('POST', '/v2/email/outbound-emails', AStream, AHeaders, nil);
    // The signed headers list should include content-type
    Assert.Contains(AResult, 'content-type;host;x-amz-content-sha256;x-amz-date');
  finally
    AStream.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

procedure TTestCanonicalRequestContentType.TestCanonical_NoQueryString_EmptyLine;
var
  AService: TTestAwsService;
  AHeaders: TStringList;
  AStream: TStringStream;
  AResult: string;
  ALines: TStringList;
begin
  // When no query params are passed, the canonical request should have
  // an empty query string line (just LF after the URI)
  AService := TTestAwsService.Create('key', 'secret', awsUsEast1);
  AHeaders := TStringList.Create;
  AStream := TStringStream.Create('', TEncoding.UTF8);
  try
    AHeaders.Values['host'] := 'email.us-east-1.amazonaws.com';
    AHeaders.Values['x-amz-date'] := '20250101T120000Z';
    AHeaders.Values['x-amz-content-sha256'] := C_EMPTY_HASH;
    (AHeaders as TStringList).Sort;

    AResult := AService.TestGenerateCanonicalRequest('GET', '/v2/email/identities', AStream, AHeaders, nil);
    // Split by LF and check: line 0=verb, line 1=URI, line 2=query string (should be empty)
    ALines := TStringList.Create;
    try
      ALines.LineBreak := #10;
      ALines.Text := AResult;
      Assert.AreEqual('GET', ALines[0], 'Line 0 should be the verb');
      Assert.AreEqual('/v2/email/identities', ALines[1], 'Line 1 should be the URI');
      Assert.AreEqual('', ALines[2], 'Line 2 should be empty (no query params)');
    finally
      ALines.Free;
    end;
  finally
    AStream.Free;
    AHeaders.Free;
    AService.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestStringToRegion);
  TDUnitX.RegisterTestFixture(TTestBaseServiceProperties);
  TDUnitX.RegisterTestFixture(TTestCanonicalRequest);
  TDUnitX.RegisterTestFixture(TTestAuthHeaderFormat);
  TDUnitX.RegisterTestFixture(TTestCanonicalRequestContentType);

end.
