require('coinmarketcapr')

get_global_marketcap('MXN')

a = get_marketcap_ticker_all(currency = "MXN")

plot_top_5_currencies('MXN')


require('devtools')
install_github("jessevent/crypto")

require('crypto')

