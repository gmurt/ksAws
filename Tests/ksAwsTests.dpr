program ksAwsTests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  {$IFNDEF TESTINSIGHT}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  Xml.Win.msxmldom,
  ksAwsConst in '..\ksAwsConst.pas',
  ksAwsHash in '..\ksAwsHash.pas',
  ksAwsHttpIntf in '..\ksAwsHttpIntf.pas',
  ksAwsHttpIndy in '..\ksAwsHttpIndy.pas',
  ksAwsBase in '..\ksAwsBase.pas',
  ksAwsS3 in '..\ksAwsS3.pas',
  ksAwsSes in '..\ksAwsSes.pas',
  ksAwsSns in '..\ksAwsSns.pas',
  ksAwsEc2 in '..\ksAwsEc2.pas',
  ksAwsRDS in '..\ksAwsRDS.pas',
  TestksAwsHash in 'TestksAwsHash.pas',
  TestksAwsBase in 'TestksAwsBase.pas',
  TestksAwsHttpIntf in 'TestksAwsHttpIntf.pas',
  TestksAwsS3 in 'TestksAwsS3.pas',
  TestksAwsSes in 'TestksAwsSes.pas',
  TestksAwsSns in 'TestksAwsSns.pas',
  TestksAwsEc2 in 'TestksAwsEc2.pas',
  TestksAwsRDS in 'TestksAwsRDS.pas';

{ keep comment here to protect the following conditional from being removed by the IDE when adding a unit }
{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    runner.FailsOnNoAsserts := False;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    System.Write('Done.. press <Enter> key to quit.');
    System.Readln;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.
