(* SPDX-License-Identifier: MIT *)
(*
   slack_pas(SlackAPI for Pascal) version 0.0.1
   Copyright 2022 YOSHIDA, Masahiro.
 *)
unit Slack.Webhook;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  fphttpclient, opensslsockets
{$IFnDEF FPC}
{$ELSE}
{$ENDIF}
  ;


type

  { TSlackWebhookClient }

  TSlackWebhookClient = class
  private
    FWebHookUrl: utf8string;
    FClient: TFPHttpClient;

    procedure SetWebHookUrl(AValue: utf8string);
  protected
  public
    {
      Incoming Webhook Post.
      @param AUrl Webhook URL.
      @param ABody JSON string.
      @param AResponse response stream.
    }
    class procedure SimplePost(const AUrl, ABody: utf8string; const AResponse: TStream);

    constructor Create;
    destructor Destroy; override;

    {
      Incoming Webhook Post.
      @param AUrl Webhook URL.
      @param ABody JSON.
      @param AResponse response stream.
    }
    procedure Post(const ABody: utf8string; const AResponse: TStream);

    // webhook URL.
    property WebHookUrl: utf8string read FWebHookUrl write SetWebHookUrl;
  end;



function SlackBuildSimpleTextBody(const Text: utf8string): utf8string;


implementation

uses
{$IFnDEF FPC}
{$ELSE}
  fpjson;
{$ENDIF}

function SlackBuildSimpleTextBody(const Text: utf8string): utf8string;
var
  obj: TJSONObject;
begin
  Result := EmptyStr;
  obj := TJSONObject.Create;
  try
    obj.Add('text', Text);
	Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

{ TSlackWebhookClient }

procedure TSlackWebhookClient.SetWebHookUrl(AValue: utf8string);
begin
  if FWebHookUrl=AValue then Exit;
  FWebHookUrl:=AValue;
end;

class procedure TSlackWebhookClient.SimplePost(const AUrl, ABody: utf8string;
  const AResponse: TStream);
begin
  with Self.Create do begin
    try
      WebhookURL := AUrl;
      Post(ABody, AResponse);
    finally
      Free;
    end;
  end;
end;

constructor TSlackWebhookClient.Create;
begin
  FWebHookUrl := EmptyStr;
  FClient := TFPHttpClient.Create(nil);
  FClient.AddHeader('Content-Type','application/json; charset=UTF-8');
  FClient.KeepConnection := false;
end;

destructor TSlackWebhookClient.Destroy;
begin
  FClient.Free;
  inherited Destroy;
end;

procedure TSlackWebhookClient.Post(const ABody: utf8string;
  const AResponse: TStream);
begin
  FClient.RequestBody := TRawByteStringStream.Create(ABody);
  try
    FClient.Post(FWebhookUrl, AResponse);
  finally
    FClient.RequestBody.Free;
  end;
end;

end.

