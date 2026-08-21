# Iframe Modal Centering Solution

## Problem
Modals displayed inside an iframe are constrained to the iframe boundaries and cannot be perfectly centered on the parent page screen.

## Solution
Modals are now rendered in the **parent page** instead of the child iframe, allowing them to be perfectly centered on the full screen.

## How It Works

### Architecture
1. **Child iframe** detects when a Bootstrap modal should be shown
2. **Child sends message** to parent via iframe-resizer's `sendMessage()` API
3. **Parent receives message** and renders modal in a fixed-position overlay
4. **Parent sends action back** to child when user interacts with modal

### Implementation Details

#### Parent Side (`index.html`)
- Added modal container with fixed positioning (z-index: 9999)
- Modal backdrop covers entire screen
- Modal dialog is centered using flexbox
- Listens for `showModal` and `hideModal` messages from iframe

#### Child Side (`aqbat/www/scripts/utilities.js`)
- Intercepts Bootstrap modal `show.bs.modal` events
- Extracts modal content (title, body, buttons)
- Sends modal data to parent via `window.parentIFrame.sendMessage()`
- Prevents default modal display in child
- Handles action responses from parent

## Usage

### Automatic (Current Implementation)
All Bootstrap modals are automatically intercepted and shown in parent. No code changes needed for existing modals.

### Manual Control
If you need to manually show a modal in the parent:

```javascript
showModalInParent(
  'my-modal-id',                    // Modal ID
  'Modal Title',                     // Title text
  '<p>Modal body content</p>',      // Body HTML
  [                                  // Buttons array
    {
      text: 'Continue',
      className: 'btn btn-primary',
      action: 'reloadWithClearedParam',  // Action to trigger
      data: {}                       // Additional data
    },
    {
      text: 'Cancel',
      className: 'btn btn-secondary',
      action: 'close',               // Closes modal
      data: {}
    }
  ],
  {                                  // Options
    size: 'large',                   // 'normal' or 'large'
    closeText: 'Close'
  }
);
```

### Supported Actions
- `'close'` - Closes the modal
- `'reload'` - Reloads the page
- `'reloadWithClearedParam'` - Reloads with cleared parameter
- `'custom'` - Executes custom onclick handler

## Modal Sizes

- **Normal**: 600px width (default)
- **Large**: 900px width (use `size: 'large'` in options)

## Styling

The parent modal uses Bootstrap-compatible classes:
- `.parent-modal-backdrop` - Dark overlay
- `.parent-modal` - Modal container
- `.parent-modal-dialog` - Dialog wrapper
- `.parent-modal-content` - Content box
- `.parent-modal-header` - Header (styled with primary color)
- `.parent-modal-body` - Body content
- `.parent-modal-footer` - Footer with buttons

## Browser Compatibility

- Works with all modern browsers
- Uses iframe-resizer v4.3.9 message API
- Falls back to `postMessage` if iframe-resizer API unavailable

## Security Considerations

The current implementation uses `'*'` as the target origin for `postMessage`. For production, consider:

1. **Restrict message origin** in parent:
```javascript
window.addEventListener('message', function(event) {
  // Verify origin
  if (event.origin !== 'https://your-iframe-domain.com') return;
  // ... handle message
});
```

2. **Use iframe-resizer's built-in security** (recommended):
The `window.parentIFrame.sendMessage()` API respects iframe-resizer's `targetOrigin` configuration.

## Troubleshooting

### Modals still showing in iframe
- Check browser console for JavaScript errors
- Verify `utilities.js` is loaded in child
- Ensure jQuery is loaded before `utilities.js`

### Modals not appearing in parent
- Check browser console for message errors
- Verify iframe-resizer is initialized: `iFrameResize({ log: true }, '#shinyIframe')`
- Check that `window.parentIFrame.sendMessage` exists

### Actions not working
- Verify action handlers are defined in `utilities.js`
- Check that button `data-action` attributes are set correctly
- Look for JavaScript errors in console

## References

- [iframe-resizer Documentation](https://github.com/davidjbradshaw/iframe-resizer)
- [iframe-resizer Message API](https://github.com/davidjbradshaw/iframe-resizer#messaging)



