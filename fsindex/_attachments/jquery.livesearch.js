/*
	jQuery live search plugin
	Version 1.0 (05/20/2009)

	Author: Jeremy Herrman (jherrman@sei.cmu.edu)

	About:
		This plugin enables ordinary text inputs to have live seach capabilities.
		As a user types, the plugin calls a specified function.

		Features:
			Query Delay:
				Only executes the callback after a delay (default is 250ms) so that 
				fast typers won't drown your website with too many calls.

			Minimum Search Length:
				Specify a minimum search length (default is 3 characters) so that 
				your system doesn't get incomplete and broad searches.

			Initial text:
				Display initial text (default is "Search") that is automatically 
				cleared when a user focuses on the text input.
				Style this by using the class "inactive_search".

			Multiple Instances:
				This plugin written so that you can have multiple live search text 
				inputs on the same page.

	Usage:

		$('#textfield').livesearch({
			searchCallback: searchFunction,
			queryDelay: 250,
			innerText: "Search",
			minimumSearchLength: 3
		});

		function searchFunction(searchTerm) {
			//do the search, update the page, etc.
		}

	Dual licensed under the MIT and GPL licenses:
	http://www.opensource.org/licenses/mit-license.php
	http://www.gnu.org/licenses/gpl.html

*/



(function($) {

	var LiveSearch = function(element, opts)
	{
		element = $(element);
		var obj = this;
		var settings = $.extend({}, $.fn.livesearch.defaults, opts);
		
		var timer = undefined;
		var prevSearchTerm = element.val();

		element.empty();

		element.bind("keyup", function() {
			// have a timer that gets canceled if a key is pressed before it executes
			if(timer != undefined) {
				clearTimeout(timer);
			}
			timer = setTimeout(DoSearch, settings.queryDelay);
		});

		this.DoSearch = DoSearch;
		function DoSearch() {
			var searchTerm = element.val();
			if(searchTerm != prevSearchTerm) {
				prevSearchTerm = searchTerm;
				if(searchTerm.length >= settings.minimumSearchLength) {
//					DisplayProgressIndication();
					DisplayResults(searchTerm);
				}
				else if(searchTerm.length == 0) {
					DisplayResults("");
				}
			}
		};

//		function DisplayProgressIndication() {
//			console.log("wait");
//		};

		function DisplayResults(searchTerm) {
			timer = undefined;
//			console.log("livesearch - " + searchTerm);
			settings.searchCallback(searchTerm);
		};

		if (element.val() == "" || element.val() == settings.innerText) {
			disableSearch();
		}
		else {
			enableSearch();
		}
		
		element.focus(function() {
			if (element.hasClass("inactive_search")) { enableSearch(); }
		});

		element.blur(function() {
			if (element.val() == "") { disableSearch(); }
		});

		function enableSearch() {
			element.addClass("active_search");
			element.removeClass("inactive_search");
			element.val("");
		};

		function disableSearch() {
			element.addClass("inactive_search");
			element.removeClass("active_search");
			element.val(settings.innerText);
		};

	};

	$.fn.livesearch = function(options)
	{
		this.each(function()
		{
			var element = $(this);

			// Return early if this element already has a plugin instance
			if (element.data('livesearch')) return;

			// pass options to plugin constructor
			var livesearch = new LiveSearch(this, options);

			// Store plugin object in this element's data
			element.data('livesearch', livesearch);
		});
	};


	$.fn.livesearch.defaults = {
		queryDelay: 250,
		innerText: "Search",
		minimumSearchLength: 3
	};

})(jQuery);
