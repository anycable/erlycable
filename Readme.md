[![Build Status](https://travis-ci.org/anycable/erlycable.svg?branch=master)](https://travis-ci.org/anycable/erlycable) 

ErlyCable – Anycable Erlang Server
=====

WebSocket server for [Anycable](https://github.com/anycable/anycable).

## Installation

TBD.

## Usage

TBD.

## Build

TBD.

## Testing

```shell
rebar3 eunit && rebar3 ct
```

## ActionCable Compatibility

Feature                  | Status 
-------------------------|--------
Connection Identifiers   | +
Connection Request (cookies, params) | +
Disconnect Handling | +
Subscribe to channels | +
Parameterized subscriptions | +
Unsubscribe from channels | +
Performing Channel Actions | +
Streaming | +
Usage of the same stream name for different channels | +
Broadcasting | +
[Custom stream callbacks](http://edgeapi.rubyonrails.org/classes/ActionCable/Channel/Streams.html) | -
[Subscription Instance Variables](http://edgeapi.rubyonrails.org/classes/ActionCable/Channel/Streams.html) | -

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/anycable/erlycable.

## License
The library is available as open source under the terms of the [MIT License](http://opensource.org/licenses/MIT).