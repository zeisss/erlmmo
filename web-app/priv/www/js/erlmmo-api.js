/**
 * Authenticates the user and gives the sessionkey back. The sessionkey is then provided to the given callback function.
 */
function auth_login(username, password, callback) {
    jQuery.post("/apiv1/auth/login",
        "username=" + encodeURIComponent(username) + "&password=" + encodeURIComponent(password),
        function(json) {
            callback(json.sessionkey, json.timeout);
        },
        "json"
    );
}

function auth_logoff (sessionkey, callback) {
    jQuery.post("/apiv1/auth/logoff", "sessionkey=" + encodeURIComponent(sessionkey), function(result) {
        if (callback != null) {
            callback(result);
        }
    }, "json");
}



function event_get(sessionkey, callback) {
    jQuery.getJSON("/apiv1/event/get", "sessionkey=" + encodeURIComponent(sessionkey), function(result){
       for (x in result) {
            callback(result[x]);
       }
    });
}


function chat_send(sessionkey, message, callback) {
    jQuery.post("/apiv1/chat/send", "message=" + encodeURIComponent(message) + "&sessionkey=" + encodeURIComponent(sessionkey),
        function (result) {
            if (callback != null) {
                callback(result);
            }
        },
        "json"
    );
}


function test_test(callback) {
    jQuery.get("/apiv1/test/test", "", callback, "json");
}