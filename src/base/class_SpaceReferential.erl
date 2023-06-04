% Copyright (C) 2023-2023 Olivier Boudeville
%
% This file is part of the OSDL-Space library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Saturday, June 3, 2023.


% @doc A <b>most general frame of reference</b> lightweight base class.
-module(class_SpaceReferential).


-define( class_description,
		 "Lightweight base class for frames of reference of all sorts." ).


% Determines what are the direct mother classes of this class (if any):
%
% (no even a trace emitter per se)
%
-define( superclasses, [] ).


% Design notes:
%
% A referential not having a parent one is thus expressed in a canonical,
% absolute, most general one.
%
% At this level, no specific dimensions apply. So for example we deal with
% points in a space of arbitrary dimension, not 2D or 3D points.
% Child classes will further refine this.


% Describes the class-specific attributes:
-define( class_attributes, [

	{ origin, point(),
	  "the origin of this referential, expressed in its parent one, if any, "
	  "otherwise in absolute terms" },

	{ parent, maybe( any_referential() ),
	  "the parent referential (if any; and of any type) of this one" } ] ).



-type referential() :: wooper:passive_instance().
% A referential, as a mere term (rather than as a WOOPER instance, i.e. a
% process).

-type referential_pid() :: wooper:instance_pid().
% A referential, as a WOOPER process.


-type referential_id() :: count().
% A referential, as designated by an identifier, defined as a (positive)
% integer, referring (as a key) to an (implicit) table of referentials.


-type any_referential() :: referential()
						 | referential_pid()
						 | referential_id().
% Any type of reference onto a referential.


-export_type([ referential/0, referential_pid/0, referential_id/0,
			   any_referential/0 ]).


% Helper (non-static) functions:
-export([ referential_to_string/1, to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Implementation notes:



% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type point() :: point:point().



% @doc Constructs an absolute referential centered at the specified origin.
-spec construct( wooper:state(), point() ) -> wooper:state().
construct( State, Origin ) ->
	construct( State, Origin, _ParentReferential=undefined ).


% @doc Constructs a referential relative to the specified parent one, and
% centered at the specified origin of it.
%
-spec construct( wooper:state(), point(), maybe( any_referential() ) ) ->
						wooper:state().
construct( State, Origin, MaybeParentReferential ) ->
	setAttributes( State, [ { origin, Origin },
							{ parent, MaybeParentReferential } ] ).



% @doc Returns a textual description of the specified referential of any type.
-spec referential_to_string( any_referential() ) -> ustring().
referential_to_string( PassiveReferential=#state_holder{} ) ->
	wooper:execute_const_request( PassiveReferential, toString );

referential_to_string( ReferentialPid ) when is_pid( ReferentialPid ) ->
	% Not executing a request for that:
	text_utils:format( "referential ~w", [ ReferentialPid ] );

referential_to_string( ReferentialId ) ->
	text_utils:format( "referential #~B", [ ReferentialId ] ).



% @doc Returns a textual description of this referential.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	ParentStr = case ?getAttr(parent) of

		undefined ->
			"absolute referential";

		AnyParentRef ->
			text_utils:format( "referential whose parent one is ~ts",
							   [ referential_to_string( AnyParentRef ) ] )

	end,

	text_utils:format( "~ts whose origin is ~ts",
					   [ ParentStr, point:to_string( ?getAttr(origin) ) ] ).
