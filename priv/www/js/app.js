$(document).ready(function() {
  $('#add-tweet').click(function() {
    $('#add-tweet-form').toggle();
  });

  $('#add-tweet-submit').click(function() {
    var tweetMessageField = $('#add-tweet-message');
    var tweetMessageForm = $('#add-tweet-form');
    var tweetMessage = tweetMessageField.val();

    $.ajax({
      type: 'POST',
      url: '/tweets',
      contentType: 'application/json',
      data: JSON.stringify({ tweet: { avatar: "https://si0.twimg.com/profile_images/2536088319/4sl2go65was3o0km520j_reasonably_small.jpeg", message: tweetMessage }}),
      success: function(d) {
        var tweetList = $('#tweet-list');

        tweetMessageField.val('');
        tweetMessageForm.toggle();
        tweetList.prepend("<li><div class='avatar' style='background: url(" + d.tweet.avatar + "); background-size: 50px 50px;'></div><div class='message'>" + d.tweet.message + "</div></li>");
      }
    });
  });

  $.ajax({
    url: '/tweets',
    success: function(d) {
      if(d.tweets) {
        var tweetList = $('#tweet-list');

        d.tweets.reverse().forEach(function(i) {
          tweetList.append("<li><div class='avatar' style='background: url(" + i.avatar + "); background-size: 50px 50px;'></div><div class='message'>" + i.message + "</div></li>");
        });
      }
    }
  });
});