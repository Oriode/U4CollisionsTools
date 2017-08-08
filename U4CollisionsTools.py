#
#    Copyright (c) 2017
#
#    All rights reserved.
#    Redistribution and use in source and binary forms, with or without
#    modification, are permitted provided that the following conditions are met:
#
#    1.  Redistributions of source code must retain the above copyright
#   	 notice, this list of conditions and the following disclaimer.
#    2.  Redistributions in binary form must reproduce the above copyright
#   	 notice, this list of conditions and the following disclaimer in the
#   	 documentation and/or other materials provided with the distribution.
#
#    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
#    OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#	@author Oriode
#	@date 02/08/2017 (DMY)
#	@file U4CollisionsTools.py
#	@brief Blender Addon for easy creation of collisions boxes for any Unreal4 project.


bl_info = {
	"name": "U4 Collisions Tools",
	"author": "Oriode",
	"version": (1,  1),
	"blender": (2, 78, 0),
	"location": "View3D > Tools > U4 Collisions Tools",
	"description": "Tools for easy U4 Collisions Box creation in Blender.",
	"warning": "",
	"wiki_url": "",
	"tracker_url": "",
	"category": "Mesh",
}


# Import Librairies
import bpy
import math
from mathutils import Vector
from bpy.props import FloatVectorProperty
import time
import fnmatch
import re
from enum import Enum
import copy




#	@brief Main Class
class U4CollisionsTools( ):
	
	m_isCollisionRegex = re.compile('U(BX|CX|CP|SP)_')

	class CollisionType( Enum ):
		BOX = 0
		SPHERE = 1
		CAPSULE = 2
		CONVEX = 3
		SIMPLECONVEX = 4


	CollisionsPrefixList = ( 'UBX_', 'USP_', 'UCP_', 'UCX_', 'UCX_' )

	@staticmethod
	def getU4Prefix( collisionType ):
		return U4CollisionsTools.CollisionsPrefixList[ int(collisionType.value) ]

	#	@brief Deselect all
	@staticmethod
	def deselectAll():
		bpy.ops.object.select_all( action = 'DESELECT' )

	@staticmethod	
	def getChildren( ob ):
		def isParent(ob, c):
			if c.parent is not None: 
				if c.parent.name == ob.name:
					return True
			else:
				return False
		return [c for c in bpy.data.objects if isParent(ob, c)]


	@staticmethod
	def getCollisions( obList ):
		return [obj for obj in obList if U4CollisionsTools.isCollisionBox( obj ) ]


	@staticmethod
	def deleteObjList( obList ):
		U4CollisionsTools.deselectAll()
		for obj in obList:
			obj.select = True

		bpy.ops.object.delete()


	@staticmethod
	def isCollisionBox( obj ):
		return ( U4CollisionsTools.m_isCollisionRegex.match( obj.name ) != None )


	def deleteCollisionsBox( self, obj ):
		objChildList = self.getChildren( obj )
		objCollisionsList = self.getCollisions( objChildList )
		self.deleteObjList( objCollisionsList )


	@staticmethod
	def setActive( obj ):
		U4CollisionsTools.deselectAll()											# Deselect everything
		obj.select = True														# Set 'obj' selected
		bpy.context.scene.objects.active = obj									# Set 'obj' as the active object


	#	@brief Get the selected objects list
	#	@return List of selected objects
	@staticmethod
	def getSelectedObj():
		return bpy.context.selected_objects


	#	@brief Add a modifier to a specific object
	#	@param obj Object to add a modifier
	#	@param modifierType Type of the modifier to add
	#	@param modifierName Name of the modifier to add
	#	@return modifier just added
	#   @see https://docs.blender.org/api/blender_python_api_current/bpy.types.ObjectModifiers.html#bpy.types.ObjectModifiers
	@staticmethod
	def addModifier( obj, modifierType, modifierName ):
		return obj.modifiers.new( modifierName, modifierType)


	#	@brief Retrieve the 3D cursor position
	#	@return Vector of position of the 3D cursor
	#	@see https://blender.stackexchange.com/questions/39291/get-current-scene-on-blender-2-74-using-python
	@staticmethod
	def get3DCursorPos():
		currentScene = bpy.context.scene
		currentSpace = bpy.context.space_data
		return ( currentSpace if currentSpace and currentSpace.type == 'VIEW_3D' else currentScene ).cursor_location

	#	@brief Set the 3D cursor position
	#	@param pos Vector of position of the 3D cursor
	#	@see https://blender.stackexchange.com/questions/39291/get-current-scene-on-blender-2-74-using-python
	@staticmethod
	def set3DCursorPos( pos ):
		currentScene = bpy.context.scene
		currentSpace = bpy.context.space_data
		( currentSpace if currentSpace and currentSpace.type == 'VIEW_3D' else currentScene ).cursor_location = pos

	#	@brief Apply a modifier to it's associated object
	#	@param obj Object
	#	@param mod Modifier to be aplied
	@staticmethod
	def applyModifier( obj, mod ):
		self.setActive( obj )
		bpy.ops.object.modifier_apply( apply_as = 'DATA', modifier = mod.name )



	@staticmethod
	def setVisibleLayer( num ):
		for i in range(20):
			if i == num:
				bpy.context.space_data.layers[i] = True
			else:
				bpy.context.space_data.layers[i] = False

	@staticmethod
	def setVisibleLayers( boolList ):
		bpy.context.space_data.layers = boolList

	@staticmethod 
	def getVisibleLayers():
		return bpy.context.space_data.layers

	@staticmethod
	def isInVisibleLayer( obj ):
		visibleLayers = U4CollisionsTools.getVisibleLayers()
		for i in range(20):
			if obj.layers[i] == True and visibleLayers[i] == True:
				return True
		return False

	#	@brief Create collisions boxes using the U4 template from a specific object list
	#	@param objList List of objects to create collisions boxes
	#	@param collisionType type of the Collision primitive to create
	#	@param multipleOnArray Create multiple Boxes for the Array modifier
	#	@param multipleOnMirror Create multiple Boxes for the Mirror modifier
	#	@param scaleMult multiplier of the scale of Collisions boxes
	#	@param convexComplexity Complexity of resulting convex shape (only used if collisionType == CONVEX)
	#	@return List of Collisions Boxes freshly created
	def createCollisionsBoxes( self, objList, collisionType, multipleOnArray = True, multipleOnMirror = True, scaleMult = 1.0, convexComplexity = 10.0 ):

		# Check if we have a type we can handle
		if collisionType != self.CollisionType.BOX and collisionType != self.CollisionType.SPHERE and collisionType != self.CollisionType.SIMPLECONVEX and collisionType != self.CollisionType.CONVEX:
			return []
		

		# Save the current 3D Cursor position
		init3DCursorPosition = Vector( self.get3DCursorPos() )
		collisionsBoxesList = []

		for obj in objList:
			# If we have selected some Collision Box, skip them (we don't want to add Collision Box to Collisions Boxes !)
			if self.isCollisionBox( obj ):
				continue

			# Delete all the already existing Collisions Boxes for 'obj'
			self.deleteCollisionsBox( obj )

			# Duplicate the object to work on a copy
			self.setActive( obj )
			bpy.ops.object.duplicate( linked = False, mode = 'TRANSLATION' )
			objCopy = bpy.context.active_object									# Save the reference of the freshly created copy.
			objCopy.name = 'U4CT.tmp'

			# Ensure everything is enabled
			objCopy.lock_location = ( False, False, False )
			objCopy.lock_scale = ( False, False, False )
			objCopy.lock_rotation = ( False, False, False )

			# Remove constraints
			for constraint in objCopy.constraints:
				objCopy.constraints.remove( constraint )


			# Because we wants to work in World Space, clear parenting
			self.setActive( objCopy )
			bpy.ops.object.parent_clear( type = 'CLEAR_KEEP_TRANSFORM' )


			objCopy.rotation_euler = ( 0.0, 0.0, 0.0 )
			
			# Disable Mirror and Array for a correct Collision Box
			for modifier in objCopy.modifiers:
				if modifier.type == 'ARRAY':
					if multipleOnArray:
						modifier.show_viewport = False
						modifier.use_merge_vertices = False
					else:
						bpy.ops.object.modifier_apply( apply_as = 'DATA', modifier = modifier.name )
				elif modifier.type == 'MIRROR':
					if multipleOnMirror:
						modifier.show_viewport = False
						modifier.use_mirror_merge = False
					else:
						bpy.ops.object.modifier_apply( apply_as = 'DATA', modifier = modifier.name )
				else:
					bpy.ops.object.modifier_apply( apply_as = 'DATA', modifier = modifier.name )

			# Set the 3D cursor to the origin of our object
			self.setActive( objCopy )
			bpy.ops.view3d.snap_cursor_to_selected()
			initObjPositionWS = Vector( self.get3DCursorPos() )

			# Set the origin at the center of mass (Because it's the easiest way to retrieve the center of mass of a mesh)
			bpy.ops.object.origin_set( type = 'ORIGIN_GEOMETRY', center = 'BOUNDS' )

			bpy.context.scene.update()										# Update the context to recompute the dimensions
			initObjScaleWS = Vector( objCopy.scale )

			

			# We will use the 3D Cursor for placing the new box at the right position (Copying the position won't work in case of parenting)
			bpy.ops.view3d.snap_cursor_to_selected()
			relativePos = self.get3DCursorPos() - initObjPositionWS

			if collisionType == self.CollisionType.BOX:
				bpy.ops.mesh.primitive_cube_add()
			elif collisionType == self.CollisionType.SPHERE:
				# Apply scale transformation on objCopy because we will use the mesh to compute the bounding sphere
				bpy.ops.object.transform_apply( location = False, rotation = False, scale = True )

				newRadius = 0.0
				for vertex in objCopy.data.vertices:
					v = vertex.co
					#v[0] *= initObjScaleWS.x
					#v[1] *= initObjScaleWS.y
					#v[2] *= initObjScaleWS.z
					newRadius = max( ( newRadius, math.sqrt( ( v[0] * v[0] ) + ( v[1] * v[1] ) + ( v[2] * v[2] ) ) ) )				
				bpy.ops.mesh.primitive_uv_sphere_add( segments = 16, ring_count = 8, size = newRadius )
			elif collisionType == self.CollisionType.SIMPLECONVEX:
				bpy.ops.mesh.primitive_cube_add()
			elif collisionType == self.CollisionType.CONVEX:
				# Here we gonna use the objCopy (He is already active)
				bpy.ops.object.mode_set( mode = 'EDIT' )
				bpy.ops.mesh.select_all( action = 'SELECT' )
				bpy.ops.mesh.convex_hull(
					delete_unused = True, 
					use_existing_faces = True, 
					make_holes = False, 
					join_triangles = True, 
					face_threshold = 3.14, 
					shape_threshold = 3.14, 
					uvs = False, 
					vcols = False, 
					seam = False, 
					sharp = False,
					materials = False )
				bpy.ops.mesh.select_all( action = 'SELECT' )
				bpy.ops.mesh.quads_convert_to_tris()				# Triangulate the shape for an exact number of triangles
				bpy.ops.object.mode_set( mode = 'OBJECT' )
				
				# Now lets create an decimate modifier for having a specified number of triangle per volume
				area = objCopy.dimensions.x * objCopy.dimensions.y * objCopy.dimensions.z
				numTriangles = len( objCopy.data.polygons )
				decimateModifier = self.addModifier( objCopy, 'DECIMATE', 'U4CT_Decimate' )
				decimateModifier.ratio = max( ( convexComplexity / ( numTriangles / area ), 1.0 / ( numTriangles / 12 ) ) )
				bpy.ops.object.modifier_apply( apply_as = 'DATA', modifier = decimateModifier.name )

			newCollisionBox = bpy.context.active_object							# Save the reference of the freshly created box.

			# Set the visibility of the new collision box same as obj
			newCollisionBox.layers = obj.layers
			
			# Set our new mesh as active
			self.setActive( newCollisionBox )

			if collisionType != self.CollisionType.SPHERE:
				objCopy.scale = ( 1.0, 1.0, 1.0 )								# Because of the Array/Mirror modifier, we will have to work with the same scale
				bpy.context.scene.update()										# Update the context to recompute the dimensions
				newCollisionBox.dimensions = objCopy.dimensions					# Set the scale of the Collision Box (will be applied)

			# Set the location at [0:0:0] because we will set it as a child of our object
			newCollisionBox.location = ( 0.0, 0.0, 0.0 )
			newCollisionBox.name = self.getU4Prefix( collisionType ) + obj.name + '.000'				# Set the name correctly using the Unreal Engine 4 documentation
			newCollisionBox.draw_type = 'WIRE'															# Set the Draw Type to WIRE

			# Disable Visibility in Cycles
			newCollisionBox.cycles_visibility.camera = False
			newCollisionBox.cycles_visibility.transmission = False
			newCollisionBox.cycles_visibility.diffuse = False
			newCollisionBox.cycles_visibility.scatter = False
			newCollisionBox.cycles_visibility.glossy = False
			newCollisionBox.cycles_visibility.shadow = False

			# Apply the scale on the new bounding box (for the array modifier)
			bpy.ops.object.transform_apply( location = False, rotation = False, scale = True )

			# Set the origin of the Collision Box same as 'obj'
			self.set3DCursorPos( Vector( ( -relativePos.x / initObjScaleWS.x, -relativePos.y / initObjScaleWS.y, -relativePos.z / initObjScaleWS.z ) ) )
			bpy.ops.object.origin_set( type = 'ORIGIN_CURSOR' )

			# Now put the collision box as a child of our object
			self.setActive( obj )
			newCollisionBox.select = True
			bpy.ops.object.parent_no_inverse_set()					# Parent it WITHOUT any 'parent inverse' matrix

			# Transfer the modifier from 'obj' to the new Collision Box			
			self.setActive( obj )
			newCollisionBox.select = True
			bpy.ops.object.make_links_data( type = 'MODIFIERS' )

			self.setActive( newCollisionBox )
			# If we are creating Simple Convex, apply a scrinkwarp
			if collisionType == self.CollisionType.SIMPLECONVEX:
				# Add a SHRINKWRAP modifier to the collision box
				shrinkWarpModifier = self.addModifier( newCollisionBox, 'SHRINKWRAP', 'U4CT_SHRINKWRAP' )
				shrinkWarpModifier.target = obj
				shrinkWarpModifier.wrap_method = 'NEAREST_VERTEX'
				bpy.ops.object.modifier_apply( apply_as = 'DATA', modifier = shrinkWarpModifier.name )
			
			
			# Apply the Array and Mirror modifier if nessessary
			self.setActive( newCollisionBox )
			if multipleOnArray:
				for modifier in newCollisionBox.modifiers:
					if modifier.type == 'ARRAY' and modifier.show_viewport == True:
						bpy.ops.object.modifier_apply( apply_as = 'DATA', modifier = modifier.name )
						
			if multipleOnMirror:
				for modifier in newCollisionBox.modifiers:
					if modifier.type == 'MIRROR' and modifier.show_viewport == True:
						modifier.use_mirror_merge = False
						bpy.ops.object.modifier_apply( apply_as = 'DATA', modifier = modifier.name )
			for modifier in newCollisionBox.modifiers:
				bpy.ops.object.modifier_remove( modifier = modifier.name )
						
			# Now split all the loose parts and rename them correctly
			bpy.ops.object.mode_set( mode = 'EDIT' )
			bpy.ops.mesh.separate( type = 'LOOSE' )
			bpy.ops.object.mode_set( mode = 'OBJECT' )
			
			def replaceDot( matchobj ):
				return matchobj.group(0).replace('.', '_')

			# All the looses parts are selected, Now set the origin to the center of mass
			bpy.ops.object.origin_set( type = 'ORIGIN_GEOMETRY', center = 'BOUNDS' )

			loosePartsObjects = bpy.context.selected_objects
			for looseObj in loosePartsObjects:
				looseObj.name = re.sub(r'\.[0-9]+$', replaceDot, looseObj.name)
				looseObj.data.name = looseObj.name
				collisionsBoxesList.append(looseObj)										# Append the loose part to the Collisions Boxes list

			# Link the object data of all the looses parts
			if collisionType == self.CollisionType.SPHERE or collisionType == self.CollisionType.BOX:
				bpy.context.scene.objects.active = newCollisionBox
				bpy.ops.object.make_links_data(type='OBDATA')

			# If we are using sphere, set the scaling equal on each axis
			if collisionType == self.CollisionType.SPHERE:
				scaleVector = Vector( ( 1.0 / initObjScaleWS.x, 1.0 / initObjScaleWS.y, 1.0 / initObjScaleWS.z ) ) * scaleMult
				for looseObj in loosePartsObjects:
					looseObj.scale = scaleVector
			else:
				newScale = Vector( ( scaleMult, scaleMult, scaleMult ) )
				for looseObj in loosePartsObjects:
					looseObj.scale = newScale

			# Everything is finished for this object, we can delete the objCopy
			if collisionType != self.CollisionType.CONVEX:
				self.setActive( objCopy )
				bpy.ops.object.delete( use_global = False )

		self.set3DCursorPos( init3DCursorPosition )

		return collisionsBoxesList






#	@brief Class of the 'Select Collisions Datas' Button
class U4CollisionsTools_SelectCollisionsBoxes(bpy.types.Operator, U4CollisionsTools):
	bl_label = 'Select Collisions Datas'							# blender will use this as a tooltip for menu items and buttons.
	bl_idname = 'object.u4collisionstools_selectallcollisionsboxes'	# unique identifier for buttons and menu items to reference.
	bl_context = '3dview'											# display name in the interface.
	bl_options = {'REGISTER', 'UNDO'}  								# enable undo for the operator.

	p_extend = bpy.props.BoolProperty(  
		name = 'Extend',
		description = 'Extend selection.',
		default = False
	)


	def execute( self, context ):
		selectedObjList = self.getSelectedObj()

		# If we don't want to extend, deselect all
		if self.p_extend == False:
			self.deselectAll()

		for obj in selectedObjList:
			objChildList = self.getChildren( obj )
			objCollisionsList = self.getCollisions( objChildList )

			for objCollisions in objCollisionsList:
				objCollisions.select = True

		return {'FINISHED'}









#	@brief Class of the 'Create Boxes' button
class U4CollisionsTools_CreateCollisionBox(bpy.types.Operator, U4CollisionsTools):
	bl_label = 'Create Boxes'										# blender will use this as a tooltip for menu items and buttons.
	bl_idname = 'object.u4collisionstools_createcollisionsboxes'	# unique identifier for buttons and menu items to reference.
	bl_context = '3dview'											# display name in the interface.
	bl_options = {'REGISTER', 'UNDO'}  								# enable undo for the operator.


	p_multipleOneMirror = bpy.props.BoolProperty(  
		name = 'Multiple on Mirror',
		description = 'Multiple Boxes generated with Mirror Modifier.',
		default = True
	)
	p_multipleOneArray = bpy.props.BoolProperty(  
		name = 'Multiple on Array',
		description = 'Multiple Boxes generated with Array Modifier.',
		default = True
	)
	p_scaleMult = bpy.props.FloatProperty(  
		name = 'Scale Multiplier',
		description = 'Multiply the resulting scale to this factor',
		default = 1.0,
		min = 0.01,
		max = 2.00
	)


	# Main method of the class:
	def execute( self, context ):
		
		collisionsBoxesList = self.createCollisionsBoxes( self.getSelectedObj(), self.CollisionType.BOX, self.p_multipleOneArray, self.p_multipleOneMirror, self.p_scaleMult )

		# Set back everything so nothing has changed
		self.deselectAll()
		for collisionsBox in collisionsBoxesList:
			collisionsBox.select = True

		return {'FINISHED'}

	

		


#	@brief Class of the 'Create Simple Convex' button
class U4CollisionsTools_CreateCollisionSimpleConvex(bpy.types.Operator, U4CollisionsTools):
	bl_label = 'Create Simple Convex'									# blender will use this as a tooltip for menu items and buttons.
	bl_idname = 'object.u4collisionstools_createcollisionssimpleconvex'	# unique identifier for buttons and menu items to reference.
	bl_context = '3dview'												# display name in the interface.
	bl_options = {'REGISTER', 'UNDO'}  									# enable undo for the operator.

	p_multipleOneMirror = bpy.props.BoolProperty(  
		name = 'Multiple on Mirror',
		description = 'Multiple Boxes generated with Mirror Modifier.',
		default = True
	)
	p_multipleOneArray = bpy.props.BoolProperty(  
		name = 'Multiple on Array',
		description = 'Multiple Boxes generated with Array Modifier.',
		default = True
	)
	p_scaleMult = bpy.props.FloatProperty(  
		name = 'Scale Multiplier',
		description = 'Multiply the resulting scale to this factor',
		default = 1.0,
		min = 0.01,
		max = 2.00
	)

	# Main method of the class:
	def execute( self, context ):
		# First we are created boxes same as 'Create Boxes'
		collisionsBoxesList = self.createCollisionsBoxes( self.getSelectedObj(), self.CollisionType.SIMPLECONVEX, self.p_multipleOneArray, self.p_multipleOneMirror, self.p_scaleMult )


		# Set back everything so nothing has changed
		self.deselectAll()
		for collisionsBox in collisionsBoxesList:
			collisionsBox.select = True

		return {'FINISHED'}






#	@brief Class of the 'Create Convex' button
class U4CollisionsTools_CreateCollisionConvex(bpy.types.Operator, U4CollisionsTools):
	bl_label = 'Create Convex'											# blender will use this as a tooltip for menu items and buttons.
	bl_idname = 'object.u4collisionstools_createcollisionsconvex'		# unique identifier for buttons and menu items to reference.
	bl_context = '3dview'												# display name in the interface.
	bl_options = {'REGISTER', 'UNDO'}  									# enable undo for the operator.

	p_multipleOneMirror = bpy.props.BoolProperty(  
		name = 'Multiple on Mirror',
		description = 'Multiple Boxes generated with Mirror Modifier.',
		default = True
	)
	p_multipleOneArray = bpy.props.BoolProperty(  
		name = 'Multiple on Array',
		description = 'Multiple Boxes generated with Array Modifier.',
		default = True
	)
	p_scaleMult = bpy.props.FloatProperty(  
		name = 'Scale Multiplier',
		description = 'Multiply the resulting scale to this factor',
		default = 1.0,
		min = 0.01,
		max = 2.00
	)
	p_complexity = bpy.props.FloatProperty(  
		name = 'Complexity',
		description = 'Complexity of the resulting convex shape',
		default = 10.0,
		min = 0.001,
		max = 1000,
		soft_min = 0.1,
		soft_max = 100.00
	)

	# Main method of the class:
	def execute( self, context ):
		# First we are created boxes same as 'Create Boxes'
		collisionsBoxesList = self.createCollisionsBoxes( self.getSelectedObj(), self.CollisionType.CONVEX, self.p_multipleOneArray, self.p_multipleOneMirror, self.p_scaleMult, self.p_complexity )


		# Set back everything so nothing has changed
		self.deselectAll()
		for collisionsBox in collisionsBoxesList:
			collisionsBox.select = True

		return {'FINISHED'}





#	@brief Class of the 'Create Spheres' button
class U4CollisionsTools_CreateCollisionSpheres(bpy.types.Operator, U4CollisionsTools):
	bl_label = 'Create Spheres'											# blender will use this as a tooltip for menu items and buttons.
	bl_idname = 'object.u4collisionstools_createcollisionsspheres'		# unique identifier for buttons and menu items to reference.
	bl_context = '3dview'												# display name in the interface.
	bl_options = {'REGISTER', 'UNDO'}  									# enable undo for the operator.

	p_multipleOneMirror = bpy.props.BoolProperty(  
		name = 'Multiple on Mirror',
		description = 'Multiple Boxes generated with Mirror Modifier.',
		default = True
	)
	p_multipleOneArray = bpy.props.BoolProperty(  
		name = 'Multiple on Array',
		description = 'Multiple Boxes generated with Array Modifier.',
		default = True
	)
	p_scaleMult = bpy.props.FloatProperty(  
		name = 'Scale Multiplier',
		description = 'Multiply the resulting scale to this factor',
		default = 1.0,
		min = 0.01,
		max = 2.00
	)

	# Main method of the class:
	def execute( self, context ):
		# First we are created boxes same as 'Create Boxes'
		collisionsBoxesList = self.createCollisionsBoxes( self.getSelectedObj(), self.CollisionType.SPHERE, self.p_multipleOneArray, self.p_multipleOneMirror, self.p_scaleMult )

		# Set back everything so nothing has changed
		self.deselectAll()
		for collisionsBox in collisionsBoxesList:
			collisionsBox.select = True

		return {'FINISHED'}







#	@brief Panel class of the Addon Used to register the whole script.
class U4CollisionsTools_Panel(bpy.types.Panel):
	bl_label = "U4 Collisions Tools"
	bl_idname = "U4CollisionsTools"
	bl_space_type = "VIEW_3D"
	bl_region_type = "TOOLS"
	bl_category = "Tools"
	first_draw = True

	def draw(self, context):
		layout = self.layout
		layout.operator(U4CollisionsTools_CreateCollisionBox.bl_idname, icon = 'VIEW3D' )
		layout.operator(U4CollisionsTools_CreateCollisionSimpleConvex.bl_idname, icon = 'VIEW3D' )
		layout.operator(U4CollisionsTools_CreateCollisionConvex.bl_idname, icon = 'VIEW3D' )
		layout.operator(U4CollisionsTools_CreateCollisionSpheres.bl_idname, icon = 'MESH_UVSPHERE' )
		layout.operator(U4CollisionsTools_SelectCollisionsBoxes.bl_idname, icon = 'HAND' )


def register():
	bpy.utils.register_class(U4CollisionsTools_Panel)
	bpy.utils.register_class(U4CollisionsTools_CreateCollisionBox)
	bpy.utils.register_class(U4CollisionsTools_CreateCollisionSimpleConvex)
	bpy.utils.register_class(U4CollisionsTools_CreateCollisionConvex)
	bpy.utils.register_class(U4CollisionsTools_CreateCollisionSpheres)
	bpy.utils.register_class(U4CollisionsTools_SelectCollisionsBoxes)

def unregister():
	bpy.utils.unregister_class(U4CollisionsTools_SelectCollisionsBoxes)
	bpy.utils.unregister_class(U4CollisionsTools_CreateCollisionSpheres)
	bpy.utils.unregister_class(U4CollisionsTools_CreateCollisionConvex)
	bpy.utils.unregister_class(U4CollisionsTools_CreateCollisionSimpleConvex)
	bpy.utils.unregister_class(U4CollisionsTools_CreateCollisionBox)
	bpy.utils.unregister_class(U4CollisionsTools_Panel)

if __name__ == "__main__":
	register()
