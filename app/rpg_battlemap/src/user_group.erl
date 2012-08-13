-module(user_group, [Id, Name, Permissions, Created, Updated]).

-export([id/0, name/0, permissions/0, created/0, updated/0]).

id() -> Id.
name() -> Name.
permissions() -> Permissions.
created() -> Created.
updated() -> Updated.
