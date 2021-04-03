module Pusher.Membership exposing (withMe, withMembers, isAdded, isRemoved)

{-| When you successfully subscribe to a Pusher Presence channel, you can ask for a list of current channel members, and updates when a member is added or removed.

@docs withMe, withMembers, isAdded, isRemoved

-}

import Json.Decode as Decode exposing (Decoder)
import Pusher.Decode exposing (eventIs)


{-| -}
withMe : Decoder member -> Decoder member
withMe decoder =
    Decode.field "me" decoder


{-| When you subscribe to a Pusher Presence channel, the `pusher:subscription_succeeded` event returns a message unlike others: instead of a `data` field, it has two new separate fields: a `me` field with information about the current user, and a `members` field with a list of information about all users currently subscribed.

The `me` field's value has a `uid` and a `data` field, as do each of the values in the `members` list.

The message has `channel` and `event` fields as usual. The `event` field is always "pusher:subscription\_succeeded", but there is no need to test for that with `eventIs`, since no other event will have a `me` or `members` field.

Here's how you might build a decoder for a `pusher:subscription_succeeded` event. Since for this particular application, the `data` field only has a single subfield, `name`, we reach into `data` to pull the name out.

    type alias Member =
        { uid : String
        , name : String
        }

    type alias MemberData =
        { channel : String
        , me : Member
        , members : List Member
        }

    memberDecoder =
        let
            memberName =
                inData "name" Decode.string
        in
        Decode.map2 Member withUid memberName

    memberDataDecoder =
        Decode.map3
            MemberData
            withChannel
            (withMe memberDecoder)
            (withMembers memberDecoder)

-}
withMembers : Decoder member -> Decoder (List member)
withMembers decoder =
    Decode.field "members" (Decode.list decoder)


{-| `isAdded` is equivalent to `eventIs "pusher:member_added"`
-}
isAdded : Decoder a -> Decoder a
isAdded =
    eventIs "pusher:member_added"


{-| `isRemoved` is equivalent to `eventIs "pusher:member_removed"`

If you've defined a `memberDecoder` for use in `withMe` and `withMembers`, you can use it as is in your decoder:

    Decode.oneOf
        [ tagMap2 Subscribers MemberData (withMe memberDecoder) (withMembers memberDecoder)
        , Decode.map MemberAdded memberDecoder |> isAdded
        , Decode.map MemberRemoved memberDecoder |> isRemoved
        ]

If you need the channel as well as the member information, you need to do a bit more work:

    type alias MemberOnChannel =
        { channel : String
        , member : Member
        }

then for member added, use

    tagMap2 MemberAdded MemberOnChannel withChannel memberDecoder |> isAdded

-}
isRemoved : Decoder a -> Decoder a
isRemoved =
    eventIs "pusher:member_removed"
