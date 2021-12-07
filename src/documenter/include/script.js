{
    'use strict';

    /** @type {HTMLInputElement} */
    const searchbar = document.getElementById('search');
    /** @type {HTMLDivElement} */
    const searchResults = document.getElementById('search-results');

    /**
     * @param {string} query 
     * @returns {string[]}
     */
    const search = (query) => window.searchIndex.filter(item => item.toLowerCase().includes(query.trim().toLowerCase()));

    document.addEventListener('keyup', event => {
        if (event.key === 's') {
            searchbar.focus();
        }
    });

    searchbar.addEventListener('keyup', event => {
        if (event.key === 'Escape') {
            searchbar.blur();
        } else if (event.key === 'Enter') {
            while (searchResults.firstChild) {
                searchResults.removeChild(searchResults.lastChild);
            }

            const results = search(searchbar.value);
            if (results.length === 0) {
                // @Temporary 
                searchResults.append('*No results*');
            } else {
                for (const result of results) {
                    const row = document.createElement('div');
                    row.textContent = result;
                    searchResults.appendChild(row);
                }
            }
            searchbar.classList.add('performed');
        }
    });

    searchbar.addEventListener('blur', _event => {
        searchbar.classList.remove('performed');
    });
}
