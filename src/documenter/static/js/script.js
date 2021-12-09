{
    'use strict';

    /** @type {HTMLInputElement} */
    const searchbar = document.getElementById('js-searchbar');
    /** @type {HTMLDivElement} */
    const searchResults = document.getElementById('js-search-results');

    /** @type {string} */
    const urlPrefix = searchbar.dataset.urlPrefix;

    /**
     * @param {string} query 
     * @returns {string[]}
     */
    const search = (query) => window.searchIndex.filter(([path, _url]) => path.toLowerCase().includes(query.trim().toLowerCase()));

    document.addEventListener('keyup', event => {
        if (event.key === 's') {
            searchbar.focus();
        }
    });

    searchbar.addEventListener('keyup', event => {
        if (event.key === 'Escape') {
            searchbar.blur();
            searchbar.classList.remove('performed');
        } else if (event.key === 'Enter') {
            // @Question is this responsive enough?
            while (searchResults.firstChild) {
                searchResults.removeChild(searchResults.lastChild);
            }

            const results = search(searchbar.value);
            if (results.length === 0) {
                // @Temporary 
                searchResults.append('*No results*');
            } else {
                for (const [path, url] of results) {
                    const row = document.createElement('div');
                    const link = document.createElement('a');
                    link.href = urlPrefix + url;
                    link.textContent = path;
                    row.appendChild(link);
                    searchResults.appendChild(row);
                }
            }
            searchbar.classList.add('performed');
        }
    });

    // searchbar.addEventListener('blur', _event => {
    //     searchbar.classList.remove('performed');
    // });
}
