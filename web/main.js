var authentication_token = null;

function addPending(albumName) {
    function makeElement(name) {
        return $('<li>' + name + ' (<a href="javascript:void(0)">edit</a>)</li>');
    }
    var element = makeElement(albumName);
    $(element).find('a').click(function() {
        var input =
                $('<input type="text" value="'+albumName+'" autofocus="true">');
        setTimeout(function() {
            var position = input[0].value.length;
            input[0].setSelectionRange(position, position);
        }, 0);
        $(input).keypress(function(e) {
            if (e.which == 13) {
                $(input).replaceWith($('<i>renaming...</i>'));
                renamePending(albumName, input[0].value);
            }
        });
        $(this).replaceWith(input);
    });
    $('#pending-container').append(element);
}

function addPermanentAlbum(albumName) {
    function makeElement(name) {
        return $('<li>' + name + '</li>');
    }
    $('#album-container').append(makeElement(albumName));
}

function fetchAlbums() {
    $.ajax({
        url: "/api/albums",
        headers: {'X-Token': authentication_token},
        success: function(data) {
            $('#pending-error').empty();
            $('#pending-container').empty();
            $('#album-container').empty();
            console.log('Got albums:');
            console.log(data);
            data.forEach(function (album) {
                if (album.pending) {
                    addPending(album.name, album.pending);
                } else {
                    addPermanentAlbum(album.name);
                }
                console.log('Added album ' + album.name);
            });
        },
        error: function(unused, unusedStatus, error) {
            $('#pending-container').empty();
            $('#pending-error').html(
                '<p><b>Error</b> fetching pending albums: ' + error);
        }
    });
}

function showError(error) {
    $('#generic-error').html(
        '<p>' + error + '</p>');
}

function renamePending(oldName, newName) {
    var request = {
        from: { name: oldName, pending: true },
        to: { name: newName, pending: false }
    };
    $.ajax({
        url: '/api/rename',
        headers: {'X-Token': authentication_token},
        type: 'POST',
        contentType: 'application/json',
        data: JSON.stringify(request),
        success: function(data) {
            if ('Left' in data) {
                showError('Error renaming ' + oldName + ' to ' + newName + ': ' +
                          data.Left);
            }
            fetchAlbums();
        }
    });
}

function onSignIn(googleUser) {
    // Useful data for your client-side scripts:
    var profile = googleUser.getBasicProfile();
    console.log("ID: " + profile.getId()); // Don't send this directly to your server!
    console.log("Name: " + profile.getName());
    console.log("Image URL: " + profile.getImageUrl());
    console.log("Email: " + profile.getEmail());
    authentication_token = googleUser.getAuthResponse().id_token;
    // The ID token you need to pass to your backend:
    var id_token = googleUser.getAuthResponse().id_token;
    console.log("ID Token: " + id_token);
    $('.g-signin2').remove();
    fetchAlbums();
};
