# slack_pas

## Overview

Slack API for pascal.  
[https://github.com/masyos/slack_pas](https://github.com/masyos/slack_pas).

## Requirement

* Lazarus 2.2.2

## Usage

### Webhook

#### Simple API

This is simple.

```Pascal
uses
  Slack.WebHook;

begin
  TSlackWebhookClient.SimplePostForTextOnly(WebHookURL, 'Hello, World.');
end;
```

## Features

## Reference

* [Slack API](https://api.slack.com/)

## Author



## License

* MIT License.

