module WebAuthn exposing (Model, Msg, initOnLogin, update, view)

import Hades exposing (LoginCredentialsResponse(..), RegisterCredentialsResponse(..), loginCredentialsEncoder, loginCredentialsResponseDecoder, registerCredentialsEncoder, registerCredentialsResponseDecoder)
import Html exposing (Html, a, br, button, div, form, input, p, span, text)
import Html.Attributes exposing (class, placeholder, style, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Maybe.Extra exposing (isJust)
import String exposing (fromInt)


type alias WithWebAuthnPort msg =
    { webauthn : ( String, String ) -> Cmd msg
    }


initOnLogin : WithWebAuthnPort msg -> String -> ( Model, Cmd msg, Cmd Msg )
initOnLogin { webauthn } lastLoginInfoFromFlags =
    let
        model =
            if lastLoginInfoFromFlags == "" then
                { username = "", password = Just "", variant = OnLogin, error = Nothing }

            else
                case Debug.log "split" <| String.split "====" lastLoginInfoFromFlags of
                    username :: "" :: [] ->
                        { username = username, password = Just "", variant = OnLogin, error = Nothing }

                    username :: [] ->
                        { username = username, password = Nothing, variant = OnLogin, error = Nothing }

                    _ ->
                        { username = "", password = Just "", variant = OnLogin, error = Nothing }
    in
    ( model
    , webauthn ( "init", js )
    , testIfSessionAuthenticated
    )


type alias Model =
    { error : Maybe String
    , variant : ModelVariant
    , username : String
    , password : Maybe String
    }


type ModelVariant
    = OnSignup
    | OnLogin


type Msg
    = UsernameChanged String
    | Login
    | Signup
    | GotCreationChallenge (Result Http.Error String)
    | GotoLogin
    | GotoSignup
    | Noop (Result Http.Error String)
    | GotRememberMeResponse (Result Http.Error String)
    | GotRegistrationResponse (Result Http.Error Hades.RegisterCredentialsResponse)
    | GotLoginResponse (Result Http.Error Hades.LoginCredentialsResponse)
    | PasswordChanged String
    | TogglePassword


update : WithWebAuthnPort Msg -> Msg -> Model -> ( Model, Cmd Msg )
update { webauthn } msg model =
    case msg of
        GotoLogin ->
            ( { model | variant = OnLogin }
            , Cmd.none
            )

        GotoSignup ->
            ( { model | variant = OnSignup }
            , Cmd.none
            )

        Noop res ->
            let
                _ =
                    Debug.log "noop" res
            in
            ( model
            , Cmd.none
            )

        GotRememberMeResponse res ->
            case res of
                Ok "yay" ->
                    ( model
                    , webauthn ( "establishSSEconnection", "" )
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        UsernameChanged username ->
            ( { model | username = username }
            , Cmd.none
            )

        TogglePassword ->
            ( { model
                | password =
                    if isJust model.password then
                        Nothing

                    else
                        Just ""
              }
            , Cmd.none
            )

        PasswordChanged newPw ->
            ( { model | password = Just newPw }
            , Cmd.none
            )

        Signup ->
            ( model
            , case model.password of
                Just password ->
                    registerWithCredentials model.username password

                Nothing ->
                    registerStart model.username
            )

        Login ->
            ( model
            , case model.password of
                Just password ->
                    loginWithCredentials model.username password

                Nothing ->
                    webauthn ( "login", model.username )
            )

        GotCreationChallenge r ->
            case r of
                Ok creationChallengeResponse ->
                    ( model
                    , webauthn ( "createCredentials", creationChallengeResponse )
                    )

                Err e ->
                    let
                        _ =
                            Debug.log "WA error:" e
                    in
                    ( model, Cmd.none )

        GotRegistrationResponse r ->
            case r of
                Ok SuccessfullyRegisteredWithCreds ->
                    ( model
                    , webauthn ( "establishSSEconnection", lastLoginInfo model )
                    )

                Ok (RegisteredWithCredsError _) ->
                    ( model
                    , Cmd.none
                    )

                Err e ->
                    let
                        _ =
                            Debug.log "WA error:" e
                    in
                    ( model, Cmd.none )

        GotLoginResponse r ->
            case r of
                Ok SuccessfullyLoggedInWithCreds ->
                    ( model
                    , webauthn ( "establishSSEconnection", lastLoginInfo model )
                    )

                Ok LoginWithCredsNotFound ->
                    ( { model | error = Just "User or password not correct" }
                    , Cmd.none
                    )

                Err e ->
                    let
                        _ =
                            Debug.log "WA error:" e
                    in
                    ( model, Cmd.none )


lastLoginInfo : Model -> String
lastLoginInfo { username, password } =
    username
        ++ (case password of
                Just _ ->
                    "===="

                _ ->
                    ""
           )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "auth" ]
        [ div [ class "cards" ] [ cardView model ] ]


cardView : Model -> Html Msg
cardView model =
    let
        rotation =
            case model.variant of
                OnLogin ->
                    0

                OnSignup ->
                    180

        username =
            model.username

        ( passwordInput, toggleLoginMethod ) =
            case model.password of
                Just pw ->
                    ( input [ onInput PasswordChanged, value pw, placeholder "password" ] []
                    , a [ onClick TogglePassword ] [ text "passwordless login" ]
                    )

                Nothing ->
                    ( text ""
                    , a [ onClick TogglePassword ] [ text "login with password" ]
                    )
    in
    div [ class "card" ]
        [ div [ class "inner", style "transform" <| "rotateY(" ++ fromInt rotation ++ "deg)" ]
            [ div [ class "front" ]
                [ div [ class "mini gameTitle" ] title
                , div [ class "form" ]
                    [ form [ onSubmit Login ]
                        [ input [ value username, placeholder "username", onInput UsernameChanged ] []
                        , passwordInput
                        , button [] [ text "login" ]
                        , if isJust model.error then
                            errorFlash model

                          else
                            br [] []
                        , toggleLoginMethod
                        , br [] []
                        , a [ onClick GotoSignup ] [ text "No account yet?" ]
                        ]
                    ]
                ]
            , div [ class "back" ]
                [ div [ class "mini gameTitle" ] [ text "Signup" ]
                , div [ class "form" ]
                    [ br [] []
                    , form [ onSubmit Signup ]
                        [ input [ value username, placeholder "username", onInput UsernameChanged ] []
                        , passwordInput
                        , button [] [ text "signup" ]
                        ]
                    , toggleLoginMethod
                    , br [] []
                    , a [ onClick GotoLogin ] [ text "Already got an account?" ]
                    , errorFlash model
                    ]
                ]
            ]
        ]


title =
    List.map
        (\s ->
            span
                (if s /= "" then
                    [ class s ]

                 else
                    []
                )
                [ text s ]
        )
        [ "E", "thereal", " ", "L", "aws", " ", "of", " ", "M", "ystery" ]


errorFlash : Model -> Html Msg
errorFlash { error } =
    case error of
        Just msg ->
            div [ class "authError" ] [ text msg ]

        Nothing ->
            text ""


registerStart : String -> Cmd Msg
registerStart username =
    Http.post
        { url = "/register_start/" ++ username
        , body = Http.emptyBody
        , expect = Http.expectString <| GotCreationChallenge
        }


registerFinish : String -> Cmd Msg
registerFinish username =
    Http.post
        { url = "/register_finish/"
        , body = Http.emptyBody
        , expect = Http.expectString <| Noop
        }


registerWithCredentials : String -> String -> Cmd Msg
registerWithCredentials username password =
    Http.post
        { url = "/register_with_credentials"
        , body = Http.jsonBody <| registerCredentialsEncoder { username = username, password = password }
        , expect = Http.expectJson GotRegistrationResponse registerCredentialsResponseDecoder
        }


loginWithCredentials : String -> String -> Cmd Msg
loginWithCredentials username password =
    Http.post
        { url = "/login_with_credentials"
        , body = Http.jsonBody <| loginCredentialsEncoder { username = username, password = password }
        , expect = Http.expectJson GotLoginResponse loginCredentialsResponseDecoder
        }


testIfSessionAuthenticated : Cmd Msg
testIfSessionAuthenticated =
    Http.get
        { url = "/remember"
        , expect = Http.expectString GotRememberMeResponse
        }


js =
    """
/**
 * Minified by jsDelivr using Terser v5.15.1.
 * Original file: /npm/js-base64@3.7.4/base64.js
 *
 * Do NOT use SRI with dynamically generated files! More information: https://www.jsdelivr.com/using-sri-with-dynamic-files
 */
!function(t,n){var r,e;"object"==typeof exports&&"undefined"!=typeof module?module.exports=n():"function"==typeof define&&define.amd?define(n):(r=t.Base64,(e=n()).noConflict=function(){return t.Base64=r,e},t.Meteor&&(Base64=e),t.Base64=e)}("undefined"!=typeof self?self:"undefined"!=typeof window?window:"undefined"!=typeof global?global:this,(function(){"use strict";var t,n="3.7.4",r="function"==typeof atob,e="function"==typeof btoa,o="function"==typeof Buffer,u="function"==typeof TextDecoder?new TextDecoder:void 0,i="function"==typeof TextEncoder?new TextEncoder:void 0,f=Array.prototype.slice.call("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="),c=(t={},f.forEach((function(n,r){return t[n]=r})),t),a=/^(?:[A-Za-z\\d+\\/]{4})*?(?:[A-Za-z\\d+\\/]{2}(?:==)?|[A-Za-z\\d+\\/]{3}=?)?$/,d=String.fromCharCode.bind(String),s="function"==typeof Uint8Array.from?Uint8Array.from.bind(Uint8Array):function(t,n){return void 0===n&&(n=function(t){return t}),new Uint8Array(Array.prototype.slice.call(t,0).map(n))},l=function(t){return t.replace(/=/g,"").replace(/[+\\/]/g,(function(t){return"+"==t?"-":"_"}))},h=function(t){return t.replace(/[^A-Za-z0-9\\+\\/]/g,"")},p=function(t){for(var n,r,e,o,u="",i=t.length%3,c=0;c<t.length;){if((r=t.charCodeAt(c++))>255||(e=t.charCodeAt(c++))>255||(o=t.charCodeAt(c++))>255)throw new TypeError("invalid character found");u+=f[(n=r<<16|e<<8|o)>>18&63]+f[n>>12&63]+f[n>>6&63]+f[63&n]}return i?u.slice(0,i-3)+"===".substring(i):u},y=e?function(t){return btoa(t)}:o?function(t){return Buffer.from(t,"binary").toString("base64")}:p,A=o?function(t){return Buffer.from(t).toString("base64")}:function(t){for(var n=[],r=0,e=t.length;r<e;r+=4096)n.push(d.apply(null,t.subarray(r,r+4096)));return y(n.join(""))},b=function(t,n){return void 0===n&&(n=!1),n?l(A(t)):A(t)},g=function(t){if(t.length<2)return(n=t.charCodeAt(0))<128?t:n<2048?d(192|n>>>6)+d(128|63&n):d(224|n>>>12&15)+d(128|n>>>6&63)+d(128|63&n);var n=65536+1024*(t.charCodeAt(0)-55296)+(t.charCodeAt(1)-56320);return d(240|n>>>18&7)+d(128|n>>>12&63)+d(128|n>>>6&63)+d(128|63&n)},B=/[\\uD800-\\uDBFF][\\uDC00-\\uDFFFF]|[^\\x00-\\x7F]/g,x=function(t){return t.replace(B,g)},C=o?function(t){return Buffer.from(t,"utf8").toString("base64")}:i?function(t){return A(i.encode(t))}:function(t){return y(x(t))},m=function(t,n){return void 0===n&&(n=!1),n?l(C(t)):C(t)},v=function(t){return m(t,!0)},U=/[\\xC0-\\xDF][\\x80-\\xBF]|[\\xE0-\\xEF][\\x80-\\xBF]{2}|[\\xF0-\\xF7][\\x80-\\xBF]{3}/g,F=function(t){switch(t.length){case 4:var n=((7&t.charCodeAt(0))<<18|(63&t.charCodeAt(1))<<12|(63&t.charCodeAt(2))<<6|63&t.charCodeAt(3))-65536;return d(55296+(n>>>10))+d(56320+(1023&n));case 3:return d((15&t.charCodeAt(0))<<12|(63&t.charCodeAt(1))<<6|63&t.charCodeAt(2));default:return d((31&t.charCodeAt(0))<<6|63&t.charCodeAt(1))}},w=function(t){return t.replace(U,F)},S=function(t){if(t=t.replace(/\\s+/g,""),!a.test(t))throw new TypeError("malformed base64.");t+="==".slice(2-(3&t.length));for(var n,r,e,o="",u=0;u<t.length;)n=c[t.charAt(u++)]<<18|c[t.charAt(u++)]<<12|(r=c[t.charAt(u++)])<<6|(e=c[t.charAt(u++)]),o+=64===r?d(n>>16&255):64===e?d(n>>16&255,n>>8&255):d(n>>16&255,n>>8&255,255&n);return o},E=r?function(t){return atob(h(t))}:o?function(t){return Buffer.from(t,"base64").toString("binary")}:S,D=o?function(t){return s(Buffer.from(t,"base64"))}:function(t){return s(E(t),(function(t){return t.charCodeAt(0)}))},R=function(t){return D(T(t))},z=o?function(t){return Buffer.from(t,"base64").toString("utf8")}:u?function(t){return u.decode(D(t))}:function(t){return w(E(t))},T=function(t){return h(t.replace(/[-_]/g,(function(t){return"-"==t?"+":"/"})))},Z=function(t){return z(T(t))},j=function(t){return{value:t,enumerable:!1,writable:!0,configurable:!0}},I=function(){var t=function(t,n){return Object.defineProperty(String.prototype,t,j(n))};t("fromBase64",(function(){return Z(this)})),t("toBase64",(function(t){return m(this,t)})),t("toBase64URI",(function(){return m(this,!0)})),t("toBase64URL",(function(){return m(this,!0)})),t("toUint8Array",(function(){return R(this)}))},O=function(){var t=function(t,n){return Object.defineProperty(Uint8Array.prototype,t,j(n))};t("toBase64",(function(t){return b(this,t)})),t("toBase64URI",(function(){return b(this,!0)})),t("toBase64URL",(function(){return b(this,!0)}))},P={version:n,VERSION:"3.7.4",atob:E,atobPolyfill:S,btoa:y,btoaPolyfill:p,fromBase64:Z,toBase64:m,encode:m,encodeURI:v,encodeURL:v,utob:x,btou:w,decode:Z,isValid:function(t){if("string"!=typeof t)return!1;var n=t.replace(/\\s+/g,"").replace(/={0,2}$/,"");return!/[^\\s0-9a-zA-Z\\+/]/.test(n)||!/[^\\s0-9a-zA-Z\\-_]/.test(n)},fromUint8Array:b,toUint8Array:R,extendString:I,extendUint8Array:O,extendBuiltins:function(){I(),O()},Base64:{}};return Object.keys(P).forEach((function(t){return P.Base64[t]=P[t]})),P}));

window.webauthnElm = {
  createCredentials: function createCredentials(jsonString) {
    const credentialCreationOptions = JSON.parse(jsonString);
    credentialCreationOptions.publicKey.challenge = Base64.toUint8Array(credentialCreationOptions.publicKey.challenge);
    credentialCreationOptions.publicKey.user.id = Base64.toUint8Array(credentialCreationOptions.publicKey.user.id);

    navigator.credentials
      .create({ publicKey: credentialCreationOptions.publicKey })
      .then((credential) => {
        fetch('/register_finish', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            id: credential.id,
            rawId: Base64.fromUint8Array(new Uint8Array(credential.rawId), true),
            type: credential.type,
            response: {
              attestationObject: Base64.fromUint8Array(new Uint8Array(credential.response.attestationObject), true),
              clientDataJSON: Base64.fromUint8Array(new Uint8Array(credential.response.clientDataJSON), true),
            },
          })
        })
          .then((response) => {
            if (response.ok){
              console.log("Successfully registered!");
            } else {
              console.log("Error whilst registering!");
            }
          });
      })

  },
  login: function login(username) {
    fetch('/login_start/' + encodeURIComponent(username), {
      method: 'POST'
    })
      .then(response => response.json())
      .then((credentialRequestOptions) => {
        credentialRequestOptions.publicKey.challenge = Base64.toUint8Array(credentialRequestOptions.publicKey.challenge);
        credentialRequestOptions.publicKey.allowCredentials.forEach(function (listItem) {
          listItem.id = Base64.toUint8Array(listItem.id)
        });

        return navigator.credentials.get({
          publicKey: credentialRequestOptions.publicKey
        });
      })
      .then((assertion) => {
        fetch('/login_finish', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify({
            id: assertion.id,
            rawId: Base64.fromUint8Array(new Uint8Array(assertion.rawId), true),
            type: assertion.type,
            response: {
              authenticatorData: Base64.fromUint8Array(new Uint8Array(assertion.response.authenticatorData), true),
              clientDataJSON: Base64.fromUint8Array(new Uint8Array(assertion.response.clientDataJSON), true),
              signature: Base64.fromUint8Array(new Uint8Array(assertion.response.signature), true),
              userHandle: Base64.fromUint8Array(new Uint8Array(assertion.response.userHandle), true)
            },
          }),
        })
          .then((response) => {
            console.log("Login response", response);
            if (response.ok){
              console.log("Successfully logged in!", response.body);
              window.webauthnElm.establishSSEconnection(username);
            } else {
              console.log("Error whilst logging in!");
            }
          });
      });
  },
  establishSSEconnection: (lastLoginInfo) => {
    window.webauthnElm.incomingPort.send(['login', '']);
    const eventSource = new EventSource('/events');
    window.webauthnElm.eventSource = eventSource;
    if (lastLoginInfo != "") {
      localStorage.setItem('lastLoginInfo', lastLoginInfo);
    }
    eventSource.onmessage = (event) => {
      window.webauthnElm.incomingPort.send(['event', event.data])
    }

  },
  portHandler: (data) => {
    if (data[0] == 'createCredentials') {
      window.webauthnElm.createCredentials(data[1]);
    }
    if (data[0] == 'login') {
      window.webauthnElm.login(data[1]);
    }
    if (data[0] == 'logout') {
      window.webauthnElm.eventSource.close();
      window.webauthnElm.eventSource = null;
    }
    if (data[0] == 'establishSSEconnection') {
      window.webauthnElm.establishSSEconnection(data[1]);
    }

  },
  getCookie: (name) => {
      const cookies = document.cookie.split(";"); // split cookies string into array
      for (let i = 0; i < cookies.length; i++) {
        const cookie = cookies[i].trim(); // trim whitespace
        const equals = cookie.indexOf("="); // find position of equals sign
        const cookieName = cookie.substring(0, equals); // get cookie name
        if (cookieName === name) {
          const cookieValue = cookie.substring(equals + 1); // get cookie value
          return decodeURIComponent(cookieValue); // decode and return cookie value
        }
      }
      return null; // return null if cookie not found
  },
  finishHoisting: (outgoingPort, incomingPort, app) => {
    app.ports[outgoingPort].unsubscribe(hoist);
    app.ports[outgoingPort].subscribe(window.webauthnElm.portHandler);
    window.webauthnElm.incomingPort = app.ports[incomingPort];
  }
};
"""
