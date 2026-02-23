unit TestksAwsHttpIntf;

interface

uses
  DUnitX.TestFramework, Classes, SysUtils;

type
  [TestFixture]
  TTestHttpResponse = class
  public
    [Test]
    procedure TestStatusCode_DefaultsToZero;
    [Test]
    procedure TestStatusCode_SetGet;
    [Test]
    procedure TestETag_SetGet;
    [Test]
    procedure TestLastModified_SetGet;
    [Test]
    procedure TestContentAsString_EmptyByDefault;
    [Test]
    procedure TestContentAsString_WithContent;
    [Test]
    procedure TestHeaderValue_SetGet;
    [Test]
    procedure TestHeaderValue_NonExistentKey;
    [Test]
    procedure TestContentStream_SetCopiesData;
  end;

implementation

uses
  ksAwsHttpIntf;

{ TTestHttpResponse }

procedure TTestHttpResponse.TestStatusCode_DefaultsToZero;
var
  AResponse: IksAwsHttpResponse;
begin
  AResponse := CreateAwsHttpResponse;
  Assert.AreEqual(0, AResponse.StatusCode);
end;

procedure TTestHttpResponse.TestStatusCode_SetGet;
var
  AResponse: IksAwsHttpResponse;
begin
  AResponse := CreateAwsHttpResponse;
  AResponse.StatusCode := 200;
  Assert.AreEqual(200, AResponse.StatusCode);
end;

procedure TTestHttpResponse.TestETag_SetGet;
var
  AResponse: IksAwsHttpResponse;
begin
  AResponse := CreateAwsHttpResponse;
  AResponse.ETag := '"abc123"';
  Assert.AreEqual('"abc123"', AResponse.ETag);
end;

procedure TTestHttpResponse.TestLastModified_SetGet;
var
  AResponse: IksAwsHttpResponse;
begin
  AResponse := CreateAwsHttpResponse;
  AResponse.LastModified := 'Wed, 09 Oct 2024 22:10:00 GMT';
  Assert.AreEqual('Wed, 09 Oct 2024 22:10:00 GMT', AResponse.LastModified);
end;

procedure TTestHttpResponse.TestContentAsString_EmptyByDefault;
var
  AResponse: IksAwsHttpResponse;
begin
  AResponse := CreateAwsHttpResponse;
  Assert.AreEqual('', AResponse.ContentAsString);
end;

procedure TTestHttpResponse.TestContentAsString_WithContent;
var
  AResponse: IksAwsHttpResponse;
  AStream: TStringStream;
begin
  AResponse := CreateAwsHttpResponse;
  AStream := TStringStream.Create('Hello World', TEncoding.UTF8);
  try
    AResponse.ContentStream := AStream;
    Assert.AreEqual('Hello World', AResponse.ContentAsString);
  finally
    AStream.Free;
  end;
end;

procedure TTestHttpResponse.TestHeaderValue_SetGet;
var
  AResponse: IksAwsHttpResponse;
begin
  AResponse := CreateAwsHttpResponse;
  AResponse.HeaderValue['x-amz-bucket-region'] := 'eu-west-2';
  Assert.AreEqual('eu-west-2', AResponse.HeaderValue['x-amz-bucket-region']);
end;

procedure TTestHttpResponse.TestHeaderValue_NonExistentKey;
var
  AResponse: IksAwsHttpResponse;
begin
  AResponse := CreateAwsHttpResponse;
  Assert.AreEqual('', AResponse.HeaderValue['nonexistent']);
end;

procedure TTestHttpResponse.TestContentStream_SetCopiesData;
var
  AResponse: IksAwsHttpResponse;
  ASource: TStringStream;
begin
  AResponse := CreateAwsHttpResponse;
  ASource := TStringStream.Create('test data', TEncoding.UTF8);
  try
    AResponse.ContentStream := ASource;
    // Verify content was copied (stream position reset to 0)
    Assert.AreEqual(Int64(0), Int64(AResponse.ContentStream.Position));
    Assert.AreEqual('test data', AResponse.ContentAsString);
  finally
    ASource.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHttpResponse);

end.
