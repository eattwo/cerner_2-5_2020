#lang racket

;; cerner_2^5_2020
(require net/url)
(require json)

(define (getSongsForArtist artist)
  (getArtistAlbums (hash-ref (makeHttpCall "https://theaudiodb.com/api/v1/json/1/searchalbum.php?s=" artist) 'album) '()))

(define (getArtistAlbums artistJson lst)
  (if (null? artistJson)
      lst
      (getArtistAlbums
       (cdr artistJson)
       (append lst (list (list
                          (hash-ref (car artistJson) 'strAlbum)
                          (getAlbumSongList (hash-ref (makeHttpCall "https://theaudiodb.com/api/v1/json/1/track.php?m=" (hash-ref (car artistJson) 'idAlbum)) 'track) '())
                          ))))))

(define (getAlbumSongList albumJson lst)
  (if (null? albumJson)
      lst
      (getAlbumSongList (cdr albumJson) (append lst (list (hash-ref (car albumJson) 'strTrack))))))

(define (makeHttpCall url id)
  (string->jsexpr (call/input-url (string->url (setUrl url id)) get-pure-port port->string)))

(define (setUrl baseUrl id)
  (cond
    [(string? id) (string-append baseUrl id)]
    [(symbol? id) (string-append baseUrl (symbol->string id))]
    [else (print "id must be a valid string or symbol.")]))