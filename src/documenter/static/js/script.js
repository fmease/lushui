{
    'use strict';

    /** @type {HTMLInputElement} */
    const searchbar = document.getElementById('searchbar');
    /** @type {HTMLDivElement} */
    const searchResults = document.getElementById('search-results');

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
                const message = document.createElement('span');
                message.classList.add('no-results');
                message.textContent = 'No results';
                searchResults.appendChild(message);
            } else {
                for (const [path, url] of results) {
                    const link = document.createElement('a');
                    link.href = urlPrefix + url;
                    link.textContent = path;
                    searchResults.appendChild(link);
                }
            }
            searchbar.classList.add('performed');
        }
    });

    // searchbar.addEventListener('blur', _event => {
    //     searchbar.classList.remove('performed');
    // });


}
