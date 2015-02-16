module View where

import VirtualDOM
import VirtualDOM.VTree
import Signal.Channel

import View.Shortcuts

type State = Number
initialState = 0


data Action = Init


view :: Channel Action -> State -> VTree
view chan st =
  div {} [
    nav {"className": "navbar navbar-inverse navbar-fixed-top"} [
       div {"className": "container"} [
          div {"className": "row"} [
             div {"className": "col-sm-3"} [
                div {"className": "navbar-header"} [
                   a {"className": "navbar-brand",
                      "href": "javascript:void(0);"} [
                      i {"className": "glyphicon glyphicon-folder-open"} []
                      ],
                   a {"className": "navbar-brand",
                      "href": "javascript:void(0);"} [
                     vtext "SlamData"
                     ]
                   ]
                ],
             div {"className": "col-sm-6"} [
               form {"className": "navbar-form"} [
                  div {"className": "input-group",
                       "style": {
                         "width": "100%"
                         }} [
                     span {"className": "input-group-addon"} [
                        vtext "Path:"
                        ],
                     input {"className": "form-control",
                            "type": "text"} [],
                     span {"className": "input-group-btn"} [
                       button {"className": "btn btn-default",
                               "type": "button"} [
                          i {"className": "glyphicon glyphicon-search"}[]
                          ]
                       ]
                     ]
                  ]
               ],
             div {"className": "col-sm-3"} [
               ul {"className": "nav navbar-nav navbar-right"} [
                  li {} [
                     a {"href": "javascript:void(0);"} [
                        vtext "User Name",
                        i {"className": "glyphicon glyphicon-user"} []
                        ]
                     ]
                  ]
               ]
             ]
          ]
       ],
    div {"className": "container"} [
      ol {"className": "breadcrumb"} [
         li {} [i {"className": "glyphicon glyphicon-chevron-right"} []],
         li {} [a {"href": "javascript:void(0);"} [vtext "DB1"]],
         li {} [a {"href": "javascript:void(0);"} [vtext "app"]]
         ],
      div {"className": "row"} [
        div {"className": "col-sm-4"} [
           i {"className": "glyphicon glyphicon-chevron-down"}[],
           vtext "Name"
           ],
        div {"className": "col-sm-8"} [
          ul {"className": "list-inline pull-right"} [
             li {} [a {"href": "javascript:void(0);"} [vtext "File"]],
             li {} [a {"href": "javascript:void(0);"} [vtext "Folder"]],
             li {} [a {"href": "javascript:void(0);"} [vtext "Mount"]],
             li {} [a {"href": "javascript:void(0);"} [vtext "Notebook"]]
             ]
          ]
        ],
      div {"className": "list-group"} [
        div {"className": "list-group-item"} [
           div {"className": "row"} [
              div {"className": "col-sm-6"} [
                 i {"className": "glyphicon glyphicon-stop"} [],
                 a {"href": "javascript:void(0);"} [vtext "Resource name"]
                 ],
              div {"className": "col-sm-6"} [
                ul {"className": "list-inline pull-right",
                    "style": {
                      "margin-bottom": 0
                      }} [
                   li {} [a {"href": "javascript:void(0);"} [vtext "configure"]],
                   li {} [a {"href": "javascript:void(0);"} [vtext "trash"]],
                   li {} [a {"href": "javascript:void(0);"} [vtext "share"]]
                   ]
                ]
              ]
           ]
        ]
      ]
    ]
