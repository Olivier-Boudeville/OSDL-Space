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
% Creation date: Sunday, June 4, 2023.


% @doc A <b>3D frame of reference</b> class.
-module(class_3DReferential).


-define( class_description,
		 "Lightweight base class for frames of reference of all sorts." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_SpaceReferential ] ).


% Design notes:
%



% Describes the class-specific attributes:
%
% (note that we do not introduce new attributes, we override their description)
%
-define( class_attributes, [
							
	{ origin, point3(),
	  "the origin of this referential, expressed in its parent one, if any, "
	  "otherwise in absolute terms" },

	{ parent, maybe( any_referential3() ),
	  "the parent referential (if any; and of any type) of this one" } ] ).



-type referential3() :: wooper:passive_instance().
% A 3D referential, as a mere term (rather than as a WOOPER instance, i.e. a
% process).

-type referential3_pid() :: wooper:instance_pid().
% A 3D referential, as a WOOPER process.


-type referential3_id() :: count().
% A 3D referential, as designated by an identifier, defined as a (positive)
% integer, referring (as a key) to an (implicit) table of 3D referentials.


-type any_referential3() :: referential3()
						  | referential3_pid()
						  | referential3_id().
% Any type of reference onto a 3D referential.


-export_type([ referential3/0, referential3_pid/0, referential3_id/0,
			   any_referential3/0 ]).


% Helper (non-static) functions:
-export([ referential3_to_string/1, to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Implementation notes:



% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type point3() :: point3:point3().



% @doc Constructs an absolute 3D referential centered at the specified origin.
-spec construct( wooper:state(), point3() ) -> wooper:state().
construct( State, Origin ) ->
	class_SpaceReferential:construct( State, Origin ).


-spec construct( wooper:state(), point3(), maybe( any_referential3() ) ) ->
						wooper:state().
construct( State, Origin, ParentReferential ) ->
	class_SpaceReferential:construct( State, Origin, ParentReferential ).



% @doc Returns a textual description of the specified referential of any type.
-spec referential3_to_string( any_referential3() ) -> ustring().
referential3_to_string( PassiveReferential=#state_holder{} ) ->
	wooper:execute_const_request( PassiveReferential, toString );

referential3_to_string( ReferentialPid ) when is_pid( ReferentialPid ) ->
	% Not executing a request for that:
	text_utils:format( "3D referential ~w", [ ReferentialPid ] );

referential3_to_string( ReferentialId ) ->
	text_utils:format( "3D referential #~B", [ ReferentialId ] ).



% @doc Returns a textual description of this referential.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	ParentStr = case ?getAttr(parent) of

		undefined ->
			"absolute 3D referential";

		AnyParentRef ->
			text_utils:format( "3D referential whose parent one is ~ts",
							   [ referential3_to_string( AnyParentRef ) ] )

	end,

	text_utils:format( "~ts whose origin is ~ts",
					   [ ParentStr, point:to_string( ?getAttr(origin) ) ] ).
