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

function auth_logout (sessionkey, callback) {
    jQuery.post("/v1/auth/logout?apikey=" + encodeURIComponent(sessionkey), "", function(result) {
        if (callback != null) {
            callback(result);
        }
    }, "json");
}



function event_get(sessionkey, callback) {
    jQuery.post("/v1/event?apikey=" + encodeURIComponent(sessionkey), "", callback, "json");
}


function chat_send(sessionkey, channel, message, callback) {
    jQuery.post(
        "/v1/chat?message=" + encodeURIComponent(message) + "&apikey=" + encodeURIComponent(sessionkey) + "&channel=" + encodeURIComponent(channel),
        "",
        function (result) {
            if (callback != null) {
                callback(result);
            }
        },
        "json"
    );
}
