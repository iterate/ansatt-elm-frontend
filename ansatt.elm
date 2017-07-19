module Ansatt exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { employees : Employees
    }


type alias Employees =
    List Employee


type Msg
    = GetEmployees (Result Http.Error Employees)


init : ( Model, Cmd Msg )
init =
    ( Model [], getEmployees )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetEmployees (Ok newEmployees) ->
            ( { model | employees = newEmployees }, Cmd.none )

        GetEmployees (Err _) ->
            ( model, Cmd.none )


css path =
    node "link" [ rel "stylesheet", href path ] []


view : Model -> Html Msg
view model =
    div []
        [ css "https://ansatt.app.iterate.no/styles/cards.css"
        , renderEmployees model.employees
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


renderEmployees : Employees -> Html Msg
renderEmployees employees =
    let
        employeeItems =
            List.map renderEmployeeCard employees
    in
        div [ class "cards has-large-padding-top-bottom" ] employeeItems


renderEmployeeCard : Employee -> Html msg
renderEmployeeCard employee =
    div [ class "employee-card" ]
        [ div [ class "employee-basic-info" ]
            [ div [ class "employee-card-header" ]
                [ img [ class "mugshot", src employee.imageUrl, title "" ]
                    []
                , ul [ class "card-content text-centered" ]
                    [ li [ class "employee-name" ]
                        [ text (String.join " " [ employee.firstName, employee.lastName ]) ]
                    , li [ class "employee-title" ]
                        [ text employee.title ]
                    ]
                ]
            , ul [ class "card-content text-centered" ]
                [ li [ class "contact-details" ]
                    [ a [ class "contact-detail", href ("mailto:" ++ employee.email) ]
                        [ text employee.email ]
                    ]
                , li [ class "contact-details" ]
                    [ a [ class "contact-detail", href ("tel:" ++ employee.phone) ]
                        [ text employee.phone ]
                    ]
                , ul [ class "horizontal-list" ]
                    [ li []
                        [ a [ class "icon", href "" ]
                            [ i [ class "fa fa-linkedin-square" ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
        ]


getEmployees =
    Http.send GetEmployees (Http.get "https://ansatt.app.iterate.no/api/people" employeesDecoder)


type alias Employee =
    { firstName : String
    , lastName : String
    , imageUrl : String
    , title : String
    , email : String
    , phone : String
    }


employeesDecoder : Decode.Decoder Employees
employeesDecoder =
    Decode.list employeeDecoder


employeeDecoder : Decode.Decoder Employee
employeeDecoder =
    Decode.map6 Employee
        (Decode.at [ "firstName" ] Decode.string)
        (Decode.at [ "lastName" ] Decode.string)
        (Decode.at [ "imageUrl" ] Decode.string)
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "email" ] Decode.string)
        (Decode.at [ "phone" ] Decode.string)
