$(document).ready(function() {
  $.ajax({
    url: '/tweets',
    success: function(d) {
      if(d.tweets) {
        var tweetList = $('#tweet-list');

        d.tweets.forEach(function(i) {
          tweetList.append("<li><div class='avatar' style='background: url(" + i.avatar + "); background-size: 50px 50px;'></div><div class='message'>" + i.message + "</div></li>");
        });
      }
    }
  });
});
