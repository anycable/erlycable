all: protos

protos:
	./_build/default/plugins/gpb/bin/protoc-erl -I./files/protos -o-erl ./src/protos -o-hrl ./src/protos anycable.proto
