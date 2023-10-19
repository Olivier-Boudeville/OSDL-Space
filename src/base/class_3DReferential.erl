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

	% Finally directly stored in its transition matrix:
	%{ origin, point3(),
	%  "the origin of this referential, expressed in its parent one, if any, "
	%  "otherwise in absolute terms" },

	{ parent, maybe( any_referential3() ),
	  "the parent referential (if any; and of any type) of this one" },

	{ to_parent, transition_matrix4(),
	  "the transition matrix from this referential to its parent; "
	  "if this referential is absolute, then it is identity_4" } ] ).



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

-type vector3() :: vector3:vector3().
-type unit_vector3() :: vector3:unit_vector3().

%-type transition_matrix4() :: matrix4:transition_matrix4().



% @doc Constructs an absolute 3D referential centered at the specified origin.
-spec construct( wooper:state(), point3() ) -> wooper:state().
construct( State, Origin ) ->
	% Not class_SpaceReferential:construct(State, Origin), as no matrix to store
	% it yet: class_SpaceReferential:construct(State, Origin),

	NoParentState = class_SpaceReferential:construct( State ),
	ParentTransMat4 = matrix4:translation( Origin ),

	setAttribute( NoParentState, to_parent, ParentTransMat4 ).



% @doc Constructs a 3D referential centered at the specified origin relatively
% to any specified one.
%
-spec construct( wooper:state(), point3(), maybe( any_referential3() ) ) ->
						wooper:state().
construct( State, Origin, MaybeParentReferential ) ->
	NoParentState = class_SpaceReferential:construct( State ),

	ParentTransMat4 = matrix4:translation( Origin ),

	setAttributes( NoParentState, [ { parent, MaybeParentReferential },
									{ to_parent, ParentTransMat4 } ] ).



% @doc Constructs a 3D referential centered at the specified origin relatively
% to the specified one, with its corresponding axes, which are supposed to be
% already unit vectors.
%
-spec construct( wooper:state(), point3(), unit_vector3(), unit_vector3(),
				 unit_vector3(), maybe( any_referential3() ) ) ->
						wooper:state().
construct( State, Origin, X, Y, Z, MaybeParentReferential ) ->

	NoParentState = class_SpaceReferential:construct( State ),

	cond_utils:if_defined( osdl_space_debug_referentials,
		begin
			point3:check( Origin ),
			vector3:check_unit_vectors( [ X, Y, Z ] ),
			vector3:check_orthogonal( X, Y ),
			vector3:check_orthogonal( X, Z ),
			vector3:check_orthogonal( Y, Z ),
			check_maybe_referential( MaybeParentReferential )
		end ),

	% From this referential to the parent one, the origin and axes of this
	% referential being expressed in its parent one:
	%
	ParentTransMat4 = matrix4:transition( Origin, X, Y, Z ),

	setAttributes( NoParentState, [ { parent, MaybeParentReferential },
									{ to_parent, ParentTransMat4 } ] ).



% @doc Constructs a 3D referential centered at the specified origin relatively
% to the specified one, with the corresponding two X and Y axes, which are not
% necessarily already unit vectors. The third axis, Z, will be deduced from X
% and Y.
%
-spec construct( wooper:state(), point3(), vector3(), vector3(),
				 maybe( any_referential3() ) ) -> wooper:state().
construct( State, Origin, X, Y, MaybeParentReferential ) ->

	NoParentState = class_SpaceReferential:construct( State ),

	cond_utils:if_defined( osdl_space_debug_referentials,
		begin
			point3:check( Origin ),
			vector3:check_vectors( [ X, Y ] ),
			check_maybe_referential( MaybeParentReferential )
		end ),

	Xunit = vector3:normalise( X ),
	Yunit = vector3:normalise( Y ),
	Zunit = vector3:cross_product( X, Y ),

	% From this referential to the parent one, the origin and axes of this
	% referential being expressed in its parent one:
	%
	ParentTransMat4 = matrix4:transition( Origin, Xunit, Yunit, Zunit ),

	setAttributes( NoParentState, [ { parent, MaybeParentReferential },
									{ to_parent, ParentTransMat4 } ] ).


% Request section.


% @doc Returns the origin of this referential relatively to its parent,
% otherwise absolutely.
%
-spec getOrigin( wooper:state() ) -> const_request_return( point3() ).
getOrigin( State ) ->
	ParentTransMat4 = ?getAttr(to_parent),
	wooper:const_return_result( matrix4:get_column_o( ParentTransMat4 ) ).



% Oneway section.


% @doc Sets the origin of this referential, relatively to its parent if any,
% otherwise absolutely.
%
-spec setOrigin( wooper:state(), point3() ) -> oneway_return().
setOrigin( State, Origin ) ->
	NewParentTransMat4 = matrix4:set_column_o( ?getAttr(to_parent), Origin ),
	NewState = setAttribute( State, to_parent, NewParentTransMat4 ),
	wooper:return_state( NewState ).




% Static section.



% @doc Checks (with necessary yet not sufficient conditions) that the specified
% term is a (3D) referential; if yes, returns it, otherwise throws an exception.
%
-spec check_referential( term() ) -> static_return( any_referential3() ).
check_referential( RefPassivInstState=#state_holder{} ) ->
	wooper:return_static(
		wooper:check_instance_of( _Classname=?MODULE, RefPassivInstState ) );

check_referential( RefPid ) when is_pid( RefPid ) ->
	wooper:return_static( RefPid );

check_referential( RefId ) when is_integer( RefId ) ->
	wooper:return_static( RefId );

check_referential( Other ) ->
	throw( { invalid_referential, Other } ).



% @doc Checks (with necessary yet not sufficient conditions) that the specified
% term is a maybe - (3D) referential; if yes, returns it, otherwise throws an
% exception.
%
-spec check_maybe_referential( term() ) -> static_return( any_referential3() ).
check_maybe_referential( MaybeRef=undefined ) ->
	wooper:return_static( MaybeRef );

check_maybe_referential( Ref ) ->
	wooper:return_static( check_referential( Ref ) ).



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
