$(document).ready(function() {
  $(document).bind("ajaxSend", function(elm, xhr, s){
    var csrf_token = $('meta[name=csrf_token]').attr('content');

    if (s.type === 'POST' || s.type === 'PUT') {
      xhr.setRequestHeader('X-CSRF-Token', csrf_token);
    }
  });

  var generateTweet = function(tweet) {
    return "<li><div class='avatar' style='background: url(" +
           tweet.avatar +
           "); background-size: auto 50px; background-position: center center;'></div><div class='message'>"
           + tweet.message + "</div></li>";
  };

  $('#add-tweet').click(function() {
    $('#add-tweet-form').toggle();
  });

  $('#add-tweet-submit').click(function() {
    var tweetMessageField = $('#add-tweet-message');
    var tweetMessageAvatar = $('#add-tweet-avatar');
    var tweetMessageForm = $('#add-tweet-form');
    var tweetMessage = tweetMessageField.val();
    var tweetAvatar = tweetMessageAvatar.val();

    $.ajax({
      type: 'POST',
      url: '/tweets',
      contentType: 'application/json',
      data: JSON.stringify({ tweet: {
        avatar: tweetAvatar,
        message: tweetMessage }}),
      success: function(d) {
        tweetMessageField.val('');
        tweetMessageForm.toggle();
      }
    });
  });

  $.ajax({
    url: '/tweets',
    success: function(d) {
      if(d.tweets) {
        var tweetList = $('#tweet-list');

        d.tweets.reverse().forEach(function(i) {
          tweetList.append(generateTweet(i));
        });

        if (!!window.EventSource) {
          var source = new EventSource('/tweets');

          source.addEventListener('message', function(e) {
            var tweetList = $('#tweet-list'), d = JSON.parse(e.data);
            tweetList.prepend(generateTweet(d));
          }, false);
        }
      }
    }
  });
});
