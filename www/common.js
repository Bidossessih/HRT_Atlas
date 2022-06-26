var HARMONIZOME = HARMONIZOME || {};

HARMONIZOME.API_URL = 'api/1.0/';

$(function () {

    /* Code executes here
     * --------------------------------------------------------------------- */
    var $searchEl = $('.search-bar');
    if ($searchEl.length) {
        setupSearch($searchEl);
    }

    setupTooltips();
    monitorSearchSelect();

    var $downloadsTools = $('.gene-set-page .downloads-tools');
    if ($downloadsTools.length) {
        setupEnrichrLink();
    }

    /* --------------------------------------------------------------------- */ 

    /* Gets the attribute and dataset from the URL.
     */
    function buildApiUrl() {
        try {
            var urlParts = window.location.pathname.split('/');
            urlParts = urlParts.splice(2);
            return HARMONIZOME.API_URL + urlParts.join('/');
        } catch (err) {
            return undefined;
        }
    }

    /* Utility function for building names for filenames and Enrichr descriptions.
     */
    function getDescription() {
        try {
            var desc,
                urlParts = window.location.pathname.split('/');
            urlParts = urlParts.splice(3);
            desc = ['harmonizome'].concat(urlParts);
            return desc.join('_');
        } catch (err) {
            return 'harmonizome';
        }
    }

    /* Bind events for sending data to Enrichr.
     */
    function setupEnrichrLink() {
        var url = buildApiUrl();
        if (typeof url != undefined) {
            $.get(url, function (data) {
                success(JSON.parse(data));
            });
        }

        function success(data) {
            var geneSymbols = [];
            data.associations.forEach(function (assoc, i) {
                geneSymbols.push(assoc.gene.symbol);
            });

            $('.enrichr').click(function () {
                var geneStr = "";
                $.each(geneSymbols, function (i, gene) {
                    geneStr += gene + "\n";
                });
                enrich({
                    description: getDescription(),
                    list: geneStr
                });
            });
        }
    }

    /* Send data to Enrichr.
     */
    function enrich(options) {
        var defaultOptions = {
            description: ""
        };

        $.extend(defaultOptions, options);

        var form = document.createElement('form');
        form.setAttribute('method', 'post');
        form.setAttribute('action', 'http://amp.pharm.mssm.edu/Enrichr/enrich');
        form.setAttribute('target', '_blank');
        form.setAttribute('enctype', 'multipart/form-data');

        var listField = document.createElement('input');
        listField.setAttribute('type', 'hidden');
        listField.setAttribute('name', 'list');
        listField.setAttribute('value', options.list);
        form.appendChild(listField);

        var descField = document.createElement('input');
        descField.setAttribute('type', 'hidden');
        descField.setAttribute('name', 'description');
        descField.setAttribute('value', options.description);
        form.appendChild(descField);

        document.body.appendChild(form);
        form.submit();
        document.body.removeChild(form);
    }

    /* Utility function for a pure JavaScript download function.
     */
    function download(filename, text) {
        var pom = document.createElement('a');
        pom.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
        pom.setAttribute('download', filename);
        if (document.createEvent) {
            var event = document.createEvent('MouseEvents');
            event.initEvent('click', true, true);
            pom.dispatchEvent(event);
        } else {
            pom.click();
        }
    }

    /* Setups search bar.
     */
    function setupSearch($parentEl) {
        var genes = new Bloodhound({
            datumTokenizer: function (datum) {
                return Bloodhound.tokenizers.whitespace(datum.value);
            },
            queryTokenizer: Bloodhound.tokenizers.whitespace,
            remote: {
                url: HARMONIZOME.API_URL + 'suggest?q=%QUERY',
                replace: function (url, urlEncodedQuery) {
                    var type = getEntityType();
                    url = url.replace('%QUERY', urlEncodedQuery);
                    return url + '&t=' + type;
                },
                wildcard: '%QUERY'
            }
        });

        $parentEl
            .find('input')
            .typeahead(
            {
                hint: true,
                highlight: true,
                minLength: 1
            },
            {
                name: 'genes',
                source: genes.ttAdapter()
            }
        ).on('typeahead:selected', function (event, selection) {
                makeSearch(selection);
            });

        /* Now move the Typeahead suggestion box outside of the input
         * form. We do this because the input form needs
         * "overflow: hidden". See: http://jsfiddle.net/0z1uup9t/
         */
        monitorSuggestionsDropdown();
    }

    function makeSearch(query) {
        var type = getEntityType();
        window.location = 'search?q=' + query + '&t=' + type;
    }

    function getEntityType() {
        return $('.search-bar .entity-dropdown :selected').val();
    }

    /* Setup Twitter Bootstrap JS tooltips.
     */
    function setupTooltips() {
        $('[data-toggle="tooltip"]').tooltip();
    }

    /* Changes the styling of the select dropdown, depending on entity type.
     *
     * Sets the class on the search entity drop down on page load. This is
     * because the browser back button will load the page with the previously
     * selected option.
     */
    function monitorSearchSelect() {
        var $select = $('.search-bar select.entity-dropdown'),
            selectedValue = $select.find(':selected').val(),
            classes = 'all gene geneSet dataset',
            transition = 'slow';

        $select.removeClass(classes);
        $select.addClass(selectedValue, transition);
        $select.change(function (evt) {
            $(this).removeClass(classes);
            $(this).addClass($(this).val(), transition);
            placeSuggestionMenu();
        });
    }

    /* Correctly places the suggestion menu.
     */
    function placeSuggestionMenu() {
        var $ttMenu = $('.tt-menu'),
            $input = $('.search-bar .input-bar'),
            $dropdown = $('.search-bar .entity-dropdown');

        /* On small screens */
        if ($dropdown.css('display') === 'none') {
            offset = $input.offset();
            offset.top += $input.outerHeight();
        } else {
            offset = $dropdown.offset();
            // Include the padding when placing the dropdown.
            offset.top += $dropdown.outerHeight();
            offset.left += $dropdown.outerWidth();
        }

        $ttMenu.css(offset).css('width', $input.width());
    }

    /* Handle correctly displaying Twitter Typeahead dropdown on any page.
     */
    function monitorSuggestionsDropdown() {
        $('.tt-menu').appendTo('body');
        placeSuggestionMenu();
        $(window).on('resize', placeSuggestionMenu);
        setOverlayWhenTyping();
    }

    /* Highlights user typing rather than screen contents.
     */
    function setOverlayWhenTyping() {
        var $input = $('.search-bar .input-bar .tt-input');
        inNav = $('.search-bar.in-navbar').length > 0;
        $input.keyup(function () {
            var term = $input.val();
            if (term && inNav) {
                $('.wrapper').css('opacity', .1);
            } else {
                $('.wrapper').css('opacity', 1);
            }
        });
    }
});