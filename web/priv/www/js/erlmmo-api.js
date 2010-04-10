/**
 * Authenticates the user and gives the sessionkey back. The sessionkey is then provided to the given callback function.
 */
function auth_login(username, password, callback) {
    jQuery.post("/v1/auth/login?" +
        "login_name=" + encodeURIComponent(username) + "&login_password=" + encodeURIComponent(password),
        "",
        function(json) {
            callback(json.apikey);
        },
        "json"
    );
}

/**
 * Deauthenticates the user and kills the sessionkey. If a callback is provided, it is called on successfull logout.
 */
function auth_logout (sessionkey, callback) {
    jQuery.post("/v1/auth/logout?apikey=" + encodeURIComponent(sessionkey), "", function(result) {
        if (callback != null) {
            callback(result);
        }
    }, "json");
}


/**
 * Fetches all pending events from the server and pass them to the callback.
 */
function event_get(sessionkey, callback) {
    jQuery.post("/v1/" + encodeURIComponent(sessionkey) + "/event", "", callback, "json");
}

/**
 *
 */
function chat_send(sessionkey, channel, message, callback) {
    jQuery.post(
        "/v1/" + encodeURIComponent(sessionkey) + "/chat?message=" + encodeURIComponent(message) + "&channel=" + encodeURIComponent(channel),
        "",
        function (result) {
            if (callback != null) {
                callback(result);
            }
        },
        "json"
    );
}

function chat_join(sessionkey, channel, callback) {
    jQuery.post(
        "/v1/" + encodeURIComponent(sessionkey) + "/chat/" + encodeURIComponent(channel),
        "",
        function(result) {
            if ( callback != null ) {
                callback(result);
            }
        },
        "json"
    );
}


function chat_part(sessionkey, channel, callback) {
    jQuery.post(
        "/v1/" + encodeURIComponent(sessionkey) + "/chat/" + encodeURIComponent(channel) + "?action=part",
        "",
        function(result) {
            if ( callback != null ) {
                callback(result);
            }
        },
        "json"
    );
}

/**
 *
 */
function chat_get_all_channels(callback) {
    jQuery.get('/v1/channels','', callback, 'json');
}