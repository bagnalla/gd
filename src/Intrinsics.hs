module Intrinsics where

import Ast
import Symtab (Id(..))

cls :: String -> Type
cls = TClass TVoid . Id

var :: String -> Type
var = TVar . Id

-- Type, type parameters, var bindings, function bindings
builtin_type_namespaces :: [(Id, [String], [(String, Type)], [(String, Type)])]

builtin_type_namespaces =
  [ (type_name TVoid,  [], [], [])
  , (type_name TNull,  [], [], [])
  , (type_name TBool,  [], [], [])
  , (type_name TInt,   [], [], [])
  , (type_name TFloat, [], [], [])

  , (type_name TString, [], [],
     -- Methods
     [ ("begins_with", TFunc [(TBool, [TString])])
     , ("bigrams", TFunc [(cls "PoolStringArray" , [])])
     , ("c_escape", TFunc [(TString, [])])
     , ("c_unescape", TFunc [(TString, [])])
     , ("capitalize", TFunc [(TString, [])])
     , ("casecmp_to", TFunc [(TInt, [TString])])
     , ("dedent", TFunc [(TString, [])])
     , ("empty", TFunc [(TBool, [])])
     , ("ends_with", TFunc [(TBool, [TString])])
     , ("erase", TFunc [(TVoid, [TInt, TInt])])
     , ("find", TFunc [ (TInt, [TString])
                      , (TInt, [TString, TInt]) ])
     , ("find_last", TFunc [(TInt, [TString])])
     , ("findn", TFunc [ (TInt, [TString])
                       , (TInt, [TString, TInt]) ])
     , ("format", TFunc [ (TString, [TDynamic])
                        , (TString, [TDynamic, TString]) ])
     , ("get_base_dir", TFunc [(TString, [])])
     , ("get_basename", TFunc [(TString, [])])
     , ("get_extension", TFunc [(TString, [])])
     , ("get_file", TFunc [(TString, [])])
     , ("hash", TFunc [(TInt, [])])
     , ("hex_to_int", TFunc [(TInt, [])])
     , ("insert", TFunc [(TString, [TInt, TString])])
     , ("is_abs_path", TFunc [(TBool, [])])
     , ("is_rel_path", TFunc [(TBool, [])])
     , ("is_subsequence_of", TFunc [(TBool, [TString])])
     , ("is_subsequence_ofi", TFunc [(TBool, [TString])])
     , ("is_valid_float", TFunc [(TBool, [])])
     , ("is_valid_hex_number", TFunc [ (TBool, [])
                                     , (TBool, [TBool]) ])
     , ("is_valid_html_color", TFunc [(TBool, [])])
     , ("is_valid_identifier", TFunc [(TBool, [])])
     , ("is_valid_integer", TFunc [(TBool, [])])
     , ("is_valid_ip_address", TFunc [(TBool, [])])
     , ("json_escape", TFunc [(TString, [])])
     , ("left", TFunc [(TString, [TInt])])
     , ("length", TFunc [(TInt, [])])
     , ("lstrip", TFunc [(TString, [TString])])
     , ("match", TFunc [(TBool, [TString])])
     , ("matchn", TFunc [(TBool, [TString])])
     , ("md5_buffer", TFunc [(cls "PoolByteArray", [])])
     , ("md5_text", TFunc [(TString, [])])
     , ("nocasecmp_to", TFunc [(TInt, [TString])])
     , ("ord_at", TFunc [(TInt, [TInt])])
     , ("pad_decimals", TFunc [(TString, [TInt])])
     , ("pad_zeroes", TFunc [(TString, [TInt])])
     , ("percent_decode", TFunc [(TString, [])])
     , ("percent_encode", TFunc [(TString, [])])
     , ("plus_file", TFunc [(TString, [TString])])
     , ("replace", TFunc [(TString, [TString, TString])])
     , ("replacen", TFunc [(TString, [TString, TString])])
     , ("rfind", TFunc [ (TInt, [TString])
                       , (TInt, [TString, TInt]) ])
     , ("rfindn", TFunc [ (TInt, [TString, TInt])
                        , (TInt, [TString, TInt]) ])
     , ("right", TFunc [(TString, [TInt])])
     , ("rsplit",
         TFunc [ (cls "PoolStringArray", [TString])
               , (cls "PoolStringArray", [TString, TBool])
               , (cls "PoolStringArray", [TString, TBool, TInt]) ])
     , ("rstrip", TFunc [(TString, [TString])])
     , ("sha256_buffer", TFunc [(cls "PoolByteArray", [])])
     , ("sha256_text", TFunc [(TString, [])])
     , ("similarity", TFunc [(TFloat, [TString])])
     , ("split",
         TFunc [ (cls "PoolStringArray", [TString])
               , (cls "PoolStringArray", [TString, TBool])
               , (cls "PoolStringArray", [TString, TBool, TInt]) ])
     , ("split_floats",
         TFunc [ (cls "PoolRealArray", [TString])
               , (cls "PoolRealArray", [TString, TBool]) ])
     , ("strip_edges", TFunc [ (TString, [])
                             , (TString, [TBool])
                             , (TString, [TBool, TBool]) ])
     , ("substr", TFunc [(TString, [TInt, TInt])])
     , ("to_ascii", TFunc [(cls "PoolByteArray", [])])
     , ("to_float", TFunc [(TFloat, [])])
     , ("to_int", TFunc [(TInt, [])])
     , ("to_lower", TFunc [(TString, [])])
     , ("to_upper", TFunc [(TString, [])])
     , ("to_utf8", TFunc [(cls "PoolByteArray", [])])
     , ("trim_prefix", TFunc [(TString, [TString])])
     , ("trim_suffix", TFunc [(TString, [TString])])
     , ("xml_escape", TFunc [(TString, [])])
     , ("xml_unescape", TFunc [(TString, [])]) ])
  
  , (type_name TVector2, [],
     -- Properties
     [ ("x", TFloat), ("y", TFloat)
     -- Constants
     , ("ZERO", TVector2), ("ONE", TVector2), ("INF", TVector2)
     , ("LEFT", TVector2) , ("RIGHT", TVector2), ("UP", TVector2)
     , ("DOWN", TVector2) ],
     -- Methods
     [ ("abs", TFunc [(TVector2, [])])
     , ("angle", TFunc [(TFloat, [])])
     , ("angle_to", TFunc [(TFloat, [TVector2])])
     , ("angle_to_point", TFunc [(TFloat, [TVector2])])
     , ("aspect", TFunc [(TFloat, [])])
     , ("bounce", TFunc [(TVector2, [TVector2])])
     , ("ceil", TFunc [(TVector2, [])])
     , ("clamped", TFunc [(TVector2, [TFloat])])
     , ("cross", TFunc [(TFloat, [TVector2])])
     , ("cubic_interpolate",
         TFunc [(TVector2,
                  [TVector2, TVector2, TVector2, TFloat])])
     , ("distance_squared_to", TFunc [(TFloat, [TVector2])])
     , ("distance_to", TFunc [(TFloat, [TVector2])])
     , ("dot", TFunc [(TFloat, [TVector2])])
     , ("floor", TFunc [(TVector2, [])])
     , ("is_normalized", TFunc [(TBool, [])])
     , ("length", TFunc [(TFloat, [])])
     , ("length_squared", TFunc [(TFloat, [])])
     , ("linear_interpolate", TFunc [(TVector2,
                                       [TVector2, TFloat])])
     , ("normalized", TFunc [(TVector2, [])])
     , ("project", TFunc [(TVector2, [TVector2])])
     , ("reflect", TFunc [(TVector2, [TVector2])])
     , ("rotated", TFunc [(TVector2, [TFloat])])
     , ("round", TFunc [(TVector2, [])])
     , ("slerp", TFunc [(TVector2, [TVector2, TFloat])])
     , ("slide", TFunc [(TVector2, [TVector2])])
     , ("snapped", TFunc [(TVector2, [TVector2])])
     , ("tangent", TFunc [(TVector2, [])]) ])

  , (type_name TRect2, [],
     -- Properties
     [ ("end", TVector2)
     , ("position", TVector2)
     , ("size", TVector2) ],
     -- Methods
     [ ("abs", TFunc [(TRect2, [])])
     , ("clip", TFunc [(TRect2, [TRect2])])
     , ("encloses", TFunc [(TBool, [TRect2])])
     , ("expand", TFunc [(TRect2, [TVector2])])
     , ("get_area", TFunc [(TFloat, [])])
     , ("grow", TFunc [(TRect2, [TFloat])])
     , ("grow_individual", TFunc [(TRect2, [TFloat, TFloat,
                                            TFloat, TFloat])])
     , ("grow_margin", TFunc [(TRect2, [TInt, TFloat])])
     , ("has_no_area", TFunc [(TBool, [])])
     , ("has_point", TFunc [(TBool, [TVector2])])
     , ("intersects", TFunc [(TBool, [TRect2])])
     , ("merge", TFunc [(TRect2, [TRect2])]) ])

  , (type_name TVector3, [],
     -- Properties
     [ ("x", TFloat), ("y", TFloat), ("z", TFloat)
     -- Constants
     , ("AXIS_X", TInt), ("AXIS_Y", TInt), ("AXIS_Z", TInt)
     , ("ZERO", TVector3), ("ONE", TVector3), ("INF", TVector3)
     , ("LEFT", TVector3), ("RIGHT", TVector3), ("UP", TVector3)
     , ("DOWN", TVector3), ("FORWARD", TVector3), ("BACK", TVector3) ],
     -- Methods
     [ ("abs", TFunc [(TVector3, [])])
     , ("angle_to", TFunc [(TFloat, [TVector3])])
     , ("bounce", TFunc [(TVector3, [TVector3])])
     , ("ceil", TFunc [(TVector3, [])])
     , ("cross", TFunc [(TVector3, [TVector3])])
     , ("cubic_interpolate", TFunc [(TVector3, [TVector3, TVector3,
                                                TVector3, TFloat])])
     , ("distance_squared_to", TFunc [(TFloat, [TVector3])])
     , ("distance_to", TFunc [(TFloat, [TVector3])])
     , ("dot", TFunc [(TFloat, [TVector3])])
     , ("floor", TFunc [(TVector3, [])])
     , ("inverse", TFunc [(TVector3, [])])
     , ("is_normalized", TFunc [(TBool, [])])
     , ("length", TFunc [(TFloat, [])])
     , ("length_squared", TFunc [(TFloat, [])])
     , ("linear_interpolate", TFunc [(TVector3, [TVector3, TFloat])])
     , ("max_axis", TFunc [(TInt, [])])
     , ("min_axis", TFunc [(TInt, [])])
     , ("normalized", TFunc [(TVector3, [])])
     , ("outer", TFunc [(TBasis, [TVector3])])
     , ("project", TFunc [(TVector3, [TVector3])])
     , ("reflect", TFunc [(TVector3, [TVector3])])
     , ("rotated", TFunc [(TVector3, [TVector3, TFloat])])
     , ("round", TFunc [(TVector3, [])])
     , ("slerp", TFunc [(TVector3, [TVector3, TFloat])])
     , ("slide", TFunc [(TVector3, [TVector3])])
     , ("snap", TFunc [(TVector3, [TVector3])])
     , ("to_diagonal_matrix", TFunc [(TBasis, [])]) ])

  , (type_name TTransform2D, [],
     -- Properties
     [ ("origin", TVector2), ("x", TVector2), ("y", TVector2)
     -- Constants
     , ("IDENTITY", TTransform2D)
     , ("FLIP_X", TTransform2D)
     , ("FLIP_Y", TTransform2D) ],
     -- Methods
     [ ("affine_inverse", TFunc [(TTransform2D, [])])
     , ("basis_xform", TFunc [(TVector2, [TVector2])])
     , ("basis_xform_inv", TFunc [(TVector2, [TVector2])])
     , ("get_origin", TFunc [(TVector2, [])])
     , ("get_rotation", TFunc [(TFloat, [])])
     , ("get_scale", TFunc [(TVector2, [])])
     , ("interpolate_with", TFunc [(TTransform2D, [TTransform2D, TFloat])])
     , ("inverse", TFunc [(TTransform2D, [])])
     , ("orthonormalized", TFunc [(TTransform2D, [])])
     , ("rotated", TFunc [(TTransform2D, [TFloat])])
     , ("scaled", TFunc [(TTransform2D, [TVector2])])
     , ("translated", TFunc [(TTransform2D, [TVector2])])
     , ("xform", TFunc [(TDynamic, [TDynamic])])
     , ("xform_inv", TFunc [(TDynamic, [TDynamic])]) ])

  , (type_name TPlane, [],
     -- Properties
     [ ("d", TFloat), ("normal", TVector3)
     , ("x", TFloat), ("y", TFloat), ("z", TFloat)
     -- Constants
     , ("PLANE_YZ", TPlane), ("PLANE_XZ", TPlane), ("PLANE_XY", TPlane) ],
     -- Methods
     [ ("center", TFunc [(TVector3, [])])
     , ("distance_to", TFunc [(TFloat, [TVector3])])
     , ("get_any_point", TFunc [(TVector3, [])])
     , ("has_point", TFunc [ (TBool, [TVector3])
                           , (TBool, [TVector3, TFloat])])
     , ("intersect_3", TFunc [(TVector3, [TPlane, TPlane])])
     , ("intersects_ray", TFunc [(TVector3, [TVector3, TVector3])])
     , ("intersects_segment", TFunc [(TVector3, [TVector3, TVector3])])
     , ("is_point_over", TFunc [(TBool, [TVector3])])
     , ("normalized", TFunc [(TPlane, [])])
     , ("project", TFunc [(TVector3, [TVector3])]) ])

  , (type_name TQuat, [],
     -- Properties
     [ ("w", TFloat), ("x", TFloat), ("y", TFloat), ("z", TFloat)
     -- Constants
     , ("IDENTITY", TQuat) ],
     -- Methods
     [ ("cubic_slerp", TFunc [(TQuat, [TQuat, TQuat, TQuat, TFloat])])
     , ("dot", TFunc [(TFloat, [TQuat])])
     , ("get_euler", TFunc [(TVector3, [])])
     , ("inverse", TFunc [(TQuat, [])])
     , ("is_normalized", TFunc [(TBool, [])])
     , ("length", TFunc [(TFloat, [])])
     , ("length_squared", TFunc [(TFloat, [])])
     , ("normalized", TFunc [(TQuat, [])])
     , ("set_axis_angle", TFunc [(TVoid, [TVector3, TFloat])])
     , ("set_euler", TFunc [(TVoid, [TVector3])])
     , ("slerp", TFunc [(TQuat, [TQuat, TFloat])])
     , ("slerpni", TFunc [(TQuat, [TQuat, TFloat])])
     , ("xform", TFunc [(TVector3, [TVector3])]) ])

  , (type_name TAABB, [],
     -- Properties
     [ ("end", TVector3), ("position", TVector3), ("size", TVector3) ],
     -- Methods
     [ ("encloses", TFunc [(TBool, [TAABB])])
     , ("expand", TFunc [(TAABB, [TVector3])])
     , ("get_area", TFunc [(TFloat, [])])
     , ("get_endpoint", TFunc [(TVector3, [TInt])])
     , ("get_longest_axis", TFunc [(TVector3, [])])
     , ("get_longest_axis_index", TFunc [(TInt, [])])
     , ("get_longest_axis_size", TFunc [(TFloat, [])])
     , ("get_shortest_axis", TFunc [(TVector3, [])])
     , ("get_shortest_axis_index", TFunc [(TInt, [])])
     , ("get_shortest_axis_size", TFunc [(TFloat, [])])
     , ("get_support", TFunc [(TVector3, [TVector3])])
     , ("grow", TFunc [(TAABB, [TFloat])])
     , ("has_no_area", TFunc [(TBool, [])])
     , ("has_no_surface", TFunc [(TBool, [])])
     , ("has_point", TFunc [(TBool, [TVector3])])
     , ("intersection", TFunc [(TAABB, [TAABB])])
     , ("intersects", TFunc [(TBool, [TAABB])])
     , ("intersects_plane", TFunc [(TBool, [TPlane])])
     , ("intersects_segment", TFunc [(TBool, [TVector3, TVector3])])
     , ("merge", TFunc [(TAABB, [TAABB])]) ])

  , (type_name TBasis, [],
     -- Properties
     [ ("x", TVector3), ("y", TVector3), ("z", TVector3) ],
     -- Methods
     [ ("determinant", TFunc [(TFloat, [])])
     , ("get_euler", TFunc [(TVector3, [])])
     , ("get_orthogonal_index", TFunc [(TInt, [])])
     , ("get_scale", TFunc [(TFloat, [])])
     , ("inverse", TFunc [(TBasis, [])])
     , ("orthonormalized", TFunc [(TBasis, [])])
     , ("rotated", TFunc [(TBasis, [TVector3, TFloat])])
     , ("scaled", TFunc [(TBasis, [TVector3])])
     , ("slerp", TFunc [(TBasis, [TBasis, TFloat])])
     , ("tdotx", TFunc [(TFloat, [TVector3])])
     , ("tdoty", TFunc [(TFloat, [TVector3])])
     , ("tdotz", TFunc [(TFloat, [TVector3])])
     , ("transposed", TFunc [(TBasis, [])])
     , ("xform", TFunc [(TVector3, [TVector3])])
     , ("xform_inv", TFunc [(TVector3, [TVector3])]) ])

  , (type_name TTransform, [],
     -- Properties
     [ ("basis", TBasis), ("origin", TVector3)
     -- Constants
     , ("IDENTITY", TTransform), ("FLIP_X", TTransform)
     , ("FLIP_Y", TTransform), ("FLIP_Z", TTransform) ],
     -- Methods
     [ ("affine_inverse", TFunc [(TTransform, [])])
     , ("interpolate_with", TFunc [(TTransform, [TTransform, TFloat])])
     , ("inverse", TFunc [(TTransform, [])])
     , ("looking_at", TFunc [(TTransform, [TVector3, TVector3])])
     , ("orthonormalized", TFunc [(TTransform, [])])
     , ("rotated", TFunc [(TTransform, [TVector3, TFloat])])
     , ("scaled", TFunc [(TTransform, [TVector3])])
     , ("translated", TFunc [(TTransform, [TVector3])])
     , ("xform", TFunc [(TDynamic, [TDynamic])])
     , ("xform_inv", TFunc [(TDynamic, [TDynamic])]) ])

  , (type_name TColor, [],
     -- Properties
     [ ("a", TFloat), ("a8", TInt), ("b", TFloat), ("b8", TInt)
     , ("g", TFloat), ("g8", TInt), ("h", TFloat), ("r", TFloat)
     , ("r8", TInt), ("s", TFloat), ("v", TFloat) ] ++
     -- Constants
     zip
     ["gray", "aliceblue", "antiquewhite", "aqua", "aquamarine", "azure",
      "beige", "bisque", "black", "blanchedalmond", "blue", "blueviolet",
      "brown", "burlywood", "cadetblue", "chartreuse", "chocolate", "coral",
      "cornflower", "cornsilk", "crimson", "cyan", "darkblue", "darkcyan",
      "darkgoldenrod", "darkgray", "darkgreen", "darkkhaki", "darkmagenta",
      "darkolivegreen", "darkorange", "darkorchid", "darkred", "darksalmon",
      "darkseagreen", "darkslateblue", "darkslategray", "darkturquoise",
      "darkviolet", "deeppink", "deepskyblue", "dimgray", "dodgerblue",
      "firebrick", "floralwhite", "forestgreen", "fuchsia", "gainsboro",
      "ghostwhite", "gold", "goldenrod", "green", "greenyellow", "honeydew",
      "hotpink", "indianred", "indigo", "ivory", "khaki", "lavender",
      "lavenderblush", "lawngreen", "lemonchiffon", "lightblue", "lightcoral",
      "lightcyan", "lightgoldenrod", "lightgray", "lightgreen", "lightpink",
      "lightsalmon", "lightseagreen", "lightskyblue", "lightslategray",
      "lightsteelblue", "lightyellow", "lime", "limegreen", "linen", "magenta",
      "maroon", "mediumaquamarine", "mediumblue", "mediumorchid",
      "mediumpurple", "mediumseagreen", "mediumslateblue", "mediumspringgreen",
      "mediumturquoise", "mediumvioletred", "midnightblue", "mintcream",
      "mistyrose", "moccasin", "navajowhite", "navyblue", "oldlace", "olive",
      "olivedrab", "orange", "orangered", "orchid", "palegoldenrod",
      "palegreen", "paleturquoise", "palevioletred", "papayawhip", "peachpuff",
      "peru", "pink", "plum", "powderblue", "purple", "rebeccapurple", "red",
      "rosybrown", "royalblue", "saddlebrown", "salmon", "sandybrown",
      "seagreen", "seashell", "sienna", "silver", "skyblue", "slateblue",
      "slategray", "snow", "springgreen", "steelblue", "tan", "teal",
      "thistle", "tomato", "turquoise", "violet", "webgray", "webgreen",
      "webmaroon", "webpurple", "wheat", "white", "whitesmoke", "yellow",
      "yellowgreen"]
      (repeat TColor),
     -- Methods
     [ ("blend", TFunc [(TColor, [TColor])])
     , ("constrasted", TFunc [(TColor, [])])
     , ("darkened", TFunc [(TColor, [TFloat])])
     , ("from_hsv", TFunc [ (TColor, [TFloat, TFloat, TFloat])
                          , (TColor, [TFloat, TFloat, TFloat, TFloat])])
     , ("gray", TFunc [(TFloat, [])])
     , ("inverted", TFunc [(TColor, [])])
     , ("lightened", TFunc [(TColor, [TFloat])])
     , ("linear_interpolate", TFunc [(TColor, [TColor, TFloat])])
     , ("to_abgr32", TFunc [(TInt, [])])
     , ("to_abgr64", TFunc [(TInt, [])])
     , ("to_argb32", TFunc [(TInt, [])])
     , ("to_argb64", TFunc [(TInt, [])])
     , ("to_html", TFunc [ (TString, [])
                         , (TString, [TBool]) ])
     , ("to_rgba32", TFunc [(TInt, [])])
     , ("to_rgba64", TFunc [(TInt, [])]) ])

  , (type_name TNodePath, [], [],
     -- Methods
     [ ("get_as_property_path", TFunc [(TNodePath, [])])
     , ("get_concatenated_subnames", TFunc [(TString, [])])
     , ("get_name", TFunc [(TString, [TInt])])
     , ("get_name_count", TFunc [(TInt, [])])
     , ("get_subname", TFunc [(TString, [TInt])])
     , ("get_subname_count", TFunc [(TInt, [])])
     , ("is_absolute", TFunc [(TBool, [])])
     , ("is_empty", TFunc [(TBool, [])]) ])

  , (type_name TRID, [], [],
     -- Methods
     [ ("get_id", TFunc [(TInt, [])]) ])

  -- , (Id "TArray", []
  --    -- Methods
  --    [ ("append", TFunc [(TVoid, [TDynamic])])
  --    , ("back", TFunc [(TDynamic, [])])
  --    , ("bsearch", TFunc [ (TInt, [TDynamic])
  --                        , (TInt, [TDynamic, TBool]) ])
  --    , ("bsearch_custom",
  --       TFunc [ (TInt, [TDynamic, cls "Object", TString])
  --             , (TInt, [TDynamic, cls "Object", TString, TBool]) ])
  --    , ("clear", TFunc [(TVoid, [])])
  --    , ("count", TFunc [(TInt, [TDynamic])])
  --    , ("duplicate", TFunc [ (TArray TDynamic, [])
  --                          , (TArray TDynamic, [TBool]) ])
  --    , ("empty", TFunc [(TBool, [])])
  --    , ("erase", TFunc [(TVoid, [TDynamic])])
  --    , ("find", TFunc [ (TInt, [TDynamic])
  --                     , (TInt, [TDynamic, TInt]) ])
  --    , ("find_last", TFunc [(TInt, [TDynamic])])
  --    , ("front", TFunc [(TDynamic, [])])
  --    , ("has", TFunc [(TBool, [TDynamic])])
  --    , ("hash", TFunc [(TInt, [])])
  --    , ("insert", TFunc [(TVoid, [TInt, TDynamic])])
  --    , ("invert", TFunc [(TVoid, [])])
  --    , ("max", TFunc [(TDynamic, [])])
  --    , ("min", TFunc [(TDynamic, [])])
  --    , ("pop_back", TFunc [(TDynamic, [])])
  --    , ("pop_front", TFunc [(TDynamic, [])])
  --    , ("remove", TFunc [(TVoid, [TInt])])
  --    , ("resize", TFunc [(TVoid, [TInt])])
  --    , ("rfind", TFunc [ (TInt, [TDynamic])
  --                      , (TInt, [TDynamic, TInt]) ])
  --    , ("shuffle", TFunc [(TVoid, [])])
  --    , ("size", TFunc [(TInt, [])])
  --    , ("sort", TFunc [(TVoid, [])])
  --    , ("sort_custom", TFunc [(TVoid, [cls "Object", TString])]) ])

  -- Arrays are polymorphic in the type of elements they contain (type
  -- parameter "a"). In the dynamic case, "a" is instantiated with
  -- TDynamic.
  , (type_name (TArray undefined), ["a"], [],
     -- Methods
     [ ("append", TFunc [(TVoid, [var "a"])])
     , ("back", TFunc [(var "a", [])])
     , ("bsearch", TFunc [ (TInt, [var "a"])
                         , (TInt, [var "a", TBool]) ])
     , ("bsearch_custom",
        TFunc [ (TInt, [var "a", cls "Object", TString])
              , (TInt, [var "a", cls "Object", TString, TBool]) ])
     , ("clear", TFunc [(TVoid, [])])
     , ("count", TFunc [(TInt, [var "a"])])
     , ("duplicate", TFunc [ (TArray $ var "a", [])
                           , (TArray $ var "a", [TBool]) ])
     , ("empty", TFunc [(TBool, [])])
     , ("erase", TFunc [(TVoid, [var "a"])])
     , ("find", TFunc [ (TInt, [var "a"])
                      , (TInt, [var "a", TInt]) ])
     , ("find_last", TFunc [(TInt, [var "a"])])
     , ("front", TFunc [(var "a", [])])
     , ("has", TFunc [(TBool, [var "a"])])
     , ("hash", TFunc [(TInt, [])])
     , ("insert", TFunc [(TVoid, [TInt, var "a"])])
     , ("invert", TFunc [(TVoid, [])])
     , ("max", TFunc [(var "a", [])])
     , ("min", TFunc [(var "a", [])])
     , ("pop_back", TFunc [(var "a", [])])
     , ("pop_front", TFunc [(var "a", [])])
     , ("remove", TFunc [(TVoid, [TInt])])
     , ("resize", TFunc [(TVoid, [TInt])])
     , ("rfind", TFunc [ (TInt, [var "a"])
                       , (TInt, [var "a", TInt]) ])
     , ("shuffle", TFunc [(TVoid, [])])
     , ("size", TFunc [(TInt, [])])
     , ("sort", TFunc [(TVoid, [])])
     , ("sort_custom", TFunc [(TVoid, [cls "Object", TString])]) ])

  -- Dictionaries are polymorphic in the types of keys and values.
  , (type_name (TDict undefined undefined), ["k", "v"], [],
     -- Methods
     [ ("clear", TFunc [(TVoid, [])])
     , ("duplicate", TFunc [ (TDict (var "k") (var "v"), [])
                           , (TDict (var "k") (var "v"), [TBool]) ])
     , ("empty", TFunc [(TBool, [])])
     , ("erase", TFunc [(TBool, [var "k"])])
     , ("get", TFunc [ (var "v", [var "k"])
                     , (var "v", [var "k", var "v"]) ])
     , ("has", TFunc [(TBool, [var "k"])])
     , ("has_all", TFunc [(TBool, [TArray $ var "k"])])
     , ("hash", TFunc [(TInt, [])])
     , ("keys", TFunc [(TArray $ var "k", [])])
     , ("size", TFunc [(TInt, [])])
     , ("values", TFunc [(TArray $ var "v", [])]) ])
  ]


-- Constructors for built-in types.
builtin_constructors :: [(String, Type)]
builtin_constructors =
  [ ("Nil", TFunc $ [ (TNull, [TArray TDynamic])
                    , (TNull, [TDict TDynamic TDynamic])
                    , (TNull, [cls "Object"]) ] ++
            zip (repeat TNull) (return <$> builtin_types))
  , ("bool", TFunc [ (TBool, [TInt])
                   , (TBool, [TFloat])
                   , (TBool, [TString]) ])
  , ("int", TFunc [ (TInt, [TBool])
                  , (TInt, [TFloat])
                  , (TInt, [TString]) ])
  , ("float", TFunc [ (TFloat, [TBool])
                    , (TFloat, [TInt])
                    , (TFloat, [TString]) ])
  , ("string", TFunc $ [ (TString, [TArray TDynamic])
                       , (TString, [TDict TDynamic TDynamic]) ] ++
               zip (repeat TString) (return <$> builtin_types))
  , ("Vector2", TFunc [(TVector2, [TFloat, TFloat])])
  , ("Rect2", TFunc [ (TRect2, [TVector2, TVector2])
                    , (TRect2, [TFloat, TFloat, TFloat, TFloat]) ])
  , ("Vector3", TFunc [(TVector3, [TFloat, TFloat, TFloat])])
  , ("Transform2D", TFunc [ (TTransform2D, [TTransform])
                          , (TTransform2D, [TVector2, TVector2, TVector2])
                          , (TTransform2D, [TFloat, TVector2]) ])
  , ("Plane", TFunc [ (TPlane, [TFloat, TFloat, TFloat, TFloat])
                    , (TPlane, [TVector3, TVector3, TVector3])
                    , (TPlane, [TVector3, TFloat]) ])
  , ("Quat", TFunc [ (TQuat, [TBasis])
                   , (TQuat, [TVector3])
                   , (TQuat, [TVector3, TFloat])
                   , (TQuat, [TFloat, TFloat, TFloat, TFloat]) ])
  , ("AABB", TFunc [(TAABB, [TVector3, TVector3])])
  , ("Basis", TFunc [ (TBasis, [TQuat])
                    , (TBasis, [TVector3])
                    , (TBasis, [TVector3, TFloat])
                    , (TBasis, [TVector3, TVector3, TVector3]) ])
  , ("Transform", TFunc [ (TTransform, [TVector3, TVector3,
                                         TVector3, TVector3])
                        , (TTransform, [TBasis, TVector3])
                        , (TTransform, [TTransform2D])
                        , (TTransform, [TQuat])
                        , (TTransform, [TBasis]) ])
  , ("Color", TFunc [ (TColor, [TString])
                    , (TColor, [TInt])
                    , (TColor, [TFloat, TFloat, TFloat])
                    , (TColor, [TFloat, TFloat, TFloat, TFloat]) ])
  , ("NodePath", TFunc [(TNodePath, [TString])])
  , ("RID", TFunc [(TRID, [cls "Object"])])
  , ("Array", TFunc [ (TArray TColor, [TPoolColorArray])
                    , (TArray TVector3, [TPoolVector3Array])
                    , (TArray TVector2, [TPoolVector2Array])
                    , (TArray TString, [TPoolStringArray])
                    , (TArray TFloat, [TPoolRealArray])
                    , (TArray TInt, [TPoolIntArray])
                    , (TArray TDynamic, [TPoolByteArray]) ])
  , ("PoolColorArray", TFunc [(TPoolColorArray, [TArray TColor])])
  , ("PoolVector3Array", TFunc [(TPoolVector3Array, [TArray TVector3])])
  , ("PoolVector2Array", TFunc [(TPoolVector2Array, [TArray TVector2])])
  , ("PoolStringArray", TFunc [(TPoolStringArray, [TArray TString])])
  , ("PoolRealArray", TFunc [(TPoolRealArray, [TArray TFloat])])
  , ("PoolIntArray", TFunc [(TPoolIntArray, [TArray TInt])])
  , ("PoolByteArray", TFunc [(TPoolByteArray, [TArray TDynamic])]) ]


-- Built-in global functions that are not considered constant.
builtin_functions :: [(String, Type)]
builtin_functions =
  [ -- ("print", TFunc [(TVoid, [TDynamic])])
  -- , ("range", TFunc [ -- (TArray TInt, [TInt, TInt, TInt])
  --                   -- , (TArray TInt, [TInt, TInt])
  --                   -- , (TArray TInt, [TInt])
  --                     (TArray TInt, [TDynamic, TDynamic, TDynamic])
  --                   , (TArray TInt, [TDynamic, TDynamic])
  --                   , (TArray TInt, [TDynamic])])
   ("range", TFunc [ (TArray TInt, [TInt, TInt, TInt])
                    , (TArray TInt, [TInt, TInt])
                    , (TArray TInt, [TInt])
                    , (TArray TDynamic, [TDynamic, TDynamic, TDynamic])
                    , (TArray TDynamic, [TDynamic, TDynamic])
                    , (TArray TDynamic, [TDynamic])])
  -- , ("load", TFunc [(TDynamic, [TString])])
  -- , ("preload", TFunc [(TDynamic, [TString])])
  -- , ("randomize", TFunc [(TVoid, [])])
  , ("Color8", TFunc [ (TColor, [TInt, TInt, TInt, TInt])
                     , (TColor, [TInt, TInt, TInt]) ])
  , ("ColorN", TFunc [ (TColor, [TString, TFloat])
                     , (TColor, [TString]) ])
  , ("assert", TFunc [(TVoid, [TBool])])
  , ("byte2var", TFunc [(TDynamic, [TPoolByteArray])])
  , ("cartesian2polar", TFunc [(TVector2, [TFloat, TFloat])])
  , ("convert", TFunc [(TDynamic, [TDynamic, TInt])])
  , ("db2linear", TFunc [(TFloat, [TFloat])])
  , ("decimals", TFunc [(TFloat, [TFloat])])
  , ("dectime", TFunc [(TFloat, [TFloat, TFloat, TFloat])])
  , ("dict2inst", TFunc [(cls "Object", [TDict TDynamic TDynamic])])
  , ("ease", TFunc [(TFloat, [TFloat, TFloat])])
  , ("funcref", TFunc [(cls "FuncRef", [cls "Object", TString])])
  , ("get_stack", TFunc [(TArray TDynamic, [])])
  , ("hash", TFunc [(TInt, [TDynamic])])
  , ("inst2dict", TFunc [(TDict TDynamic TDynamic, [cls "Object"])])
  , ("instance_from_id", TFunc [(cls "Object", [TInt])])
  , ("inverse_lerp", TFunc [(TFloat, [TFloat, TFloat, TFloat])])
  , ("is_inf", TFunc [(TBool, [TFloat])])
  , ("is_instance_valid", TFunc [(TBool, [cls "Object"])])
  , ("is_nan", TFunc [(TBool, [TFloat])])
  , ("len", TFunc [(TInt, [TDynamic])])
  , ("lerp", TFunc [(TDynamic, [TDynamic, TDynamic, TFloat])])
  , ("linear2db", TFunc [(TFloat, [TFloat])])
  , ("load", TFunc [(cls "Resource", [TString])])
  , ("nearest_po2", TFunc [(TInt, [TInt])])
  , ("parse_json", TFunc [(TDynamic, [TString])])
  , ("polar2cartesian", TFunc [(TVector2, [TFloat, TFloat])])
  , ("preload", TFunc [(cls "Resource", [TString])])
  , ("print", TFunc [(TVoid, [TDynamic])])
  , ("print_debug", TFunc [(TVoid, [TDynamic])])
  , ("print_stack", TFunc [(TVoid, [])])
  , ("printerr", TFunc [(TVoid, [TDynamic])])
  , ("printraw", TFunc [(TVoid, [TDynamic])])
  , ("prints", TFunc [(TVoid, [TDynamic])])
  , ("printt", TFunc [(TVoid, [TDynamic])])
  , ("push_error", TFunc [(TVoid, [TString])])
  , ("push_warning", TFunc [(TVoid, [TString])])
  , ("rand_range", TFunc [(TFloat, [TFloat, TFloat])])
  , ("rand_seed", TFunc [(TArray TDynamic, [TInt])])
  , ("randf", TFunc [(TFloat, [])])
  , ("randi", TFunc [(TInt, [])])
  , ("randomize", TFunc [(TVoid, [])])
  , ("range_lerp", TFunc [(TFloat, [TFloat, TFloat, TFloat, TFloat, TFloat])])
  , ("seed", TFunc [(TVoid, [TInt])])
  , ("stepify", TFunc [(TFloat, [TFloat, TFloat])])
  , ("str", TFunc [(TString, [TDynamic])])
  , ("str2var", TFunc [(TDynamic, [TString])])
  , ("to_json", TFunc [(TString, [TDynamic])])
  , ("type_exists", TFunc [(TBool, [TDynamic])])
  , ("typeof", TFunc [(TInt, [TDynamic])])
  , ("validate_json", TFunc [(TString, [TString])])
  , ("var2bytes", TFunc [(TPoolByteArray, [TDynamic])])
  , ("var2str", TFunc [(TString, [TDynamic])])
  , ("weakref", TFunc [(cls "WeakRef", [cls "Object"])])
  , ("wrapf", TFunc [(TFloat, [TFloat, TFloat, TFloat])])
  , ("wrapi", TFunc [(TInt, [TInt, TInt, TInt])])
  , ("yield", TFunc [ (cls "GDScriptFunctionState", [cls "Object", TString])
                    , (cls "GDScriptFunctionState", [cls "Object"])
                    , (cls "GDScriptFunctionState", []) ])
  ]

-- Built-in global functions that are considered constant.
constant_functions :: [(String, Type)]
constant_functions =
  [ ("abs", TFunc [(TFloat, [TFloat])])
  , ("acos", TFunc [(TFloat, [TFloat])])
  , ("asin", TFunc [(TFloat, [TFloat])])
  , ("atan", TFunc [(TFloat, [TFloat])])
  , ("atan2", TFunc [(TFloat, [TFloat, TFloat])])
  , ("ceil", TFunc [(TFloat, [TFloat])])
  , ("char", TFunc [(TString, [TInt])])
  , ("clamp", TFunc [(TFloat, [TFloat, TFloat, TFloat])])
  , ("cos", TFunc [(TFloat, [TFloat])])
  , ("cosh", TFunc [(TFloat, [TFloat])])
  , ("deg2rad", TFunc [(TFloat, [TFloat])])
  , ("exp", TFunc [(TFloat, [TFloat])])
  , ("floor", TFunc [(TFloat, [TFloat])])
  , ("fmod", TFunc [(TFloat, [TFloat, TFloat])])
  , ("fposmod", TFunc [(TFloat, [TFloat, TFloat])])
  , ("log", TFunc [(TFloat, [TFloat])])
  , ("max", TFunc [(TFloat, [TFloat, TFloat])])
  , ("min", TFunc [(TFloat, [TFloat, TFloat])])
  , ("pow", TFunc [(TFloat, [TFloat, TFloat])])
  , ("rad2deg", TFunc [(TFloat, [TFloat])])
  , ("round", TFunc [(TFloat, [TFloat])])
  , ("sign", TFunc [(TFloat, [TFloat])])
  , ("sin", TFunc [(TFloat, [TFloat])])
  , ("sinh", TFunc [(TFloat, [TFloat])])
  , ("sqrt", TFunc [(TFloat, [TFloat])])
  , ("tan", TFunc [(TFloat, [TFloat])])
  , ("tanh", TFunc [(TFloat, [TFloat])]) ]

-- Built-in global constants.
constant_vars :: [(String, Type)]
constant_vars =
  [ ("null", TNull)
  , ("PI", TFloat)
  , ("TAU", TFloat)
  , ("INF", TFloat)
  , ("NAN", TFloat) ]
